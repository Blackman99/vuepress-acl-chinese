第十章：宏
==========

Lisp 代码是由 Lisp 对象的列表来表示。2.3 节宣称这让 Lisp
可以写出**可自己写程序的程序**。本章将示范如何跨越表达式与代码的界线。

10.1 求值 (Eval)
----------------

如何产生表达式是很直观的：调用 `list` 即可。我们没有考虑到的是，如何使
Lisp 将列表视为代码。这之间缺少的一环是函数 `eval`
，它接受一个表达式，将其求值，然后返回它的值:

    > (eval '(+ 1 2 3))
    6
    > (eval '(format t "Hello"))
    Hello
    NIL

如果这看起很熟悉的话，这是应该的。这就是我们一直交谈的那个 `eval`
。下面这个函数实现了与顶层非常相似的东西:

    (defun our-toplevel ()
      (do ()
          (nil)
        (format t "~%> ")
        (print (eval (read)))))

也是因为这个原因，顶层也称为**读取─求值─打印循环** (read-eval-print
loop, REPL)。

调用 `eval` 是跨越代码与列表界线的一种方法。但它不是一个好方法:

1.  它的效率低下： `eval` 处理的是原始列表 (raw
    list)，或者当下编译它，或者用直译器求值。两种方法都比执行编译过的代码来得慢许多。
2.  表达式在没有词法语境 (lexical
    context)的情况下被求值。举例来说，如果你在一个 `let` 里调用 `eval`
    ，传给 `eval` 的表达式将无法引用由 `let` 所设置的变量。

有许多更好的方法 (下一节叙述)来利用产生代码的这个可能性。当然 `eval`
也是有用的，唯一合法的用途像是在顶层循环使用它。

对于程序员来说， `eval` 的主要价值大概是作为 Lisp
的概念模型。我们可以想像 Lisp 是由一个长的 `cond` 表达式定义而成:

    (defun eval (expr env)
      (cond ...
            ((eql (car expr) 'quote) (cdr expr))
            ...
            (t (apply (symbol-function (car expr))
                      (mapcar #'(lambda (x)
                                  (eval x env))
                              (cdr expr))))))

许多表达式由预设子句 (default clause)来处理，预设子句获得 `car`
所引用的函数，将 `cdr` 所有的参数求值，并返回将前者应用至后者的结果。
[^1]

但是像 `(quote x)` 那样的句子就不能用这样的方式来处理，因为 `quote`
就是为了防止它的参数被求值而存在的。所以我们需要给 `quote`
写一个特别的子句。这也是为什么本质上将其称为特殊操作符 (special
operator): 一个需要被实现为 `eval` 的一个特殊情况的操作符。

函数 `coerce` 与 `compile`
提供了一个类似的桥梁，让你把列表转成代码。你可以 `coerce` 一个 lambda
表达式，使其成为函数，

    > (coerce '(lambda (x) x) 'function)
    #<Interpreted-Function BF9D96>

而如果你将 `nil` 作为第一个参数传给 `compile`
，它会编译作为第二个参数传入的 lambda 表达式。

    > (compile nil '(lambda (x) (+ x 2)))
    #<Compiled-Function BF55BE>
    NIL
    NIL

由于 `coerce` 与 `compile` 可接受列表作为参数，一个程序可以在动态执行时
(on the fly)构造新函数。但与调用 `eval`
比起来，这不是一个从根本解决的办法，并且需抱有同样的疑虑来检视这两个函数。

函数 `eval` , `coerce` 与 `compile`
的麻烦不是它们跨越了代码与列表之间的界线，而是它们在执行期做这件事。跨越界线的代价昂贵。大多数情况下，在编译期做这件事是没问题的，当你的程序执行时，几乎不用成本。下一节会示范如何办到这件事。

10.2 宏 (Macros)
----------------

写出能写程序的程序的最普遍方法是通过定义宏。*宏*是通过转换
(transformation)而实现的操作符。你通过说明你一个调用应该要翻译成什么，来定义一个宏。这个翻译称为宏展开(macro-expansion)，宏展开由编译器自动完成。所以宏所产生的代码，会变成程序的一个部分，就像你自己输入的程序一样。

宏通常通过调用 `defmacro` 来定义。一个 `defmacro` 看起来很像 `defun`
。但是与其定义一个函数调用应该产生的值，它定义了该怎么翻译出一个函数调用。举例来说，一个将其参数设为
`nil` 的宏可以定义成如下:

    (defmacro nil! (x)
      (list 'setf x nil))

这定义了一个新的操作符，称为 `nil!` ，它接受一个参数。一个这样形式
`(nil! a)` 的调用，会在求值或编译前，被翻译成 `(setf a nil)`
。所以如果我们输入 `(nil! x)` 至顶层，

    > (nil! x)
    NIL
    > x
    NIL

完全等同于输入表达式 `(setf x nil)` 。

要测试一个函数，我们调用它，但要测试一个宏，我们看它的展开式
(expansion)。

函数 `macroexpand-1` 接受一个宏调用，并产生它的展开式:

    > (macroexpand-1 '(nil! x))
    (SETF X NIL)
    T

一个宏调用可以展开成另一个宏调用。当编译器（或顶层）遇到一个宏调用时，它持续展开它，直到不可展开为止。

理解宏的秘密是理解它们是如何被实现的。在台面底下，它们只是转换成表达式的函数。举例来说，如果你传入这个形式
`(nil! a)` 的表达式给这个函数

    (lambda (expr)
      (apply #'(lambda (x) (list 'setf x nil))
             (cdr expr)))

它会返回 `(setf a nil)` 。当你使用 `defmacro`
，你定义一个类似这样的函数。 `macroexpand-1`
全部所做的事情是，当它看到一个表达式的 `car`
是宏时，将表达式传给对应的函数。

10.3 反引号 (Backquote)
-----------------------

反引号读取宏 (read-macro)使得从模版
(templates)建构列表变得有可能。反引号广泛使用在宏定义中。一个平常的引用是键盘上的右引号
(apostrophe)，然而一个反引号是一个左引号。(译注: open quote
左引号，closed quote
右引号)。它称作“反引号”是因为它看起来像是反过来的引号 (titled
backwards)。

(译注: 反引号是键盘左上方数字 1 左边那个: ``\` ，而引号是 enter 左边那个
`'`)

一个反引号单独使用时，等于普通的引号:

    > `(a b c)
    (A B C)

和普通引号一样，单一个反引号保护其参数被求值。

反引号的优点是，在一个反引号表达式里，你可以使用 `,` （逗号）与 `,@`
（comma-at）来重启求值。如果你在反引号表达式里，在某个东西前面加逗号，则它会被求值。所以我们可以使用反引号与逗号来建构列表模版:

    > (setf a 1 b 2)
    2
    > `(a is ,a and b is ,b)
    (A IS 1 AND B IS 2)

通过使用反引号取代调用 `list` ，我们可以写出会产生出展开式的宏。举例来说
`nil!` 可以定义为:

    (defmacro nil! (x)
      `(setf ,x nil))

`,@`
与逗号相似，但将（本来应该是列表的）参数扒开。将列表的元素插入模版来取代列表。

    > (setf lst '(a b c))
    (A B C)
    > `(lst is ,lst)
    (LST IS (A B C))
    > `(its elements are ,@lst)
    (ITS ELEMENTS ARE A B C)

`,@`
在宏里很有用，举例来说，在用剩余参数表示代码主体的宏。假设我们想要一个
`while` 宏，只要初始测试表达式为真，对其主体求值:

    > (let ((x 0))
        (while (< x 10)
           (princ x)
           (incf x)))
    0123456789
    NIL

我们可以通过使用一个剩余参数 (rest parameter)
，搜集主体的表达式列表，来定义一个这样的宏，接着使用 comma-at
来扒开这个列表放至展开式里:

    (defmacro while (test &rest body)
      `(do ()
           ((not ,test))
         ,@body))

10.4 示例：快速排序法(Example: Quicksort)
-----------------------------------------

图 10.1 包含了重度依赖宏的一个示例函数 ── 一个使用快速排序演算法
[λ](http://acl.readthedocs.org/en/latest/zhCN/notes-cn.html#notes-164)
来排序向量的函数。这个函数的工作方式如下:

    (defun quicksort (vec l r)
      (let ((i l)
            (j r)
            (p (svref vec (round (+ l r) 2))))    ; 1
        (while (<= i j)                           ; 2
          (while (< (svref vec i) p) (incf i))
          (while (> (svref vec j) p) (decf j))
          (when (<= i j)
            (rotatef (svref vec i) (svref vec j))
            (incf i)
            (decf j)))
        (if (>= (- j l) 1) (quicksort vec l j))    ; 3
        (if (>= (- r i) 1) (quicksort vec i r)))
      vec)

**图 10.1 快速排序。**

1.  开始你通过选择某个元素作为主键（ *pivot*
    ）。许多实现选择要被排序的序列中间元素。
2.  接着你分割（partition）向量，持续交换元素，直到所有主键左边的元素小于主键，右边的元素大于主键。
3.  最后，如果左右分割之一有两个或更多元素时，你递归地应用这个算法至向量的那些分割上。

每一次递归时，分割越变越小，直到向量完整排序为止。

在图 10.1
的实现里，接受一个向量以及标记欲排序范围的两个整数。这个范围当下的中间元素被选为主键
( `p`
)。接着从左右两端开始产生分割，并将左边太大或右边太小的元素交换过来。(将两个参数传给
`rotatef`
函数，交换它们的值。)最后，如果一个分割含有多个元素时，用同样的流程来排序它们。

除了我们前一节定义的 `while` 宏之外，图 10.1 也用了内置的 `when` ,
`incf` , `decf` 以及 `rotatef`
宏。使用这些宏使程序看起来更加简洁与清晰。

10.5 设计宏 (Macro Design)
--------------------------

撰写宏是一种独特的程序设计，它有着独一无二的目标与问题。能够改变编译器所看到的东西，就像是能够重写它一样。所以当你开始撰写宏时，你需要像语言设计者一样思考。

本节快速给出宏所牵涉问题的概要，以及解决它们的技巧。作为一个例子，我们会定义一个称为
`ntimes` 的宏，它接受一个数字 *n* 并对其主体求值 *n* 次。

    > (ntimes 10
        (princ "."))
    ..........
    NIL

下面是一个不正确的 `ntimes` 定义，说明了宏设计中的某些议题:

    (defmacro ntimes (n &rest body)
      `(do ((x 0 (+ x 1)))
           ((>= x ,n))
         ,@body))

这个定义第一眼看起来可能没问题。在上面这个情况，它会如预期的工作。但实际上它在两个方面坏掉了。

一个宏设计者需要考虑的问题之一是，不小心引入的变量捕捉 (variable
capture)。这发生在当一个在宏展开式里用到的变量，恰巧与展开式即将插入的语境里，有使用同样名字作为变量的情况。不正确的
`ntimes` 定义创造了一个变量 `x` 。所以如果这个宏在已经有 `x`
作为名字的地方被调用时，它可能无法做到我们所预期的:

    > (let ((x 10))
        (ntimes 5
           (setf x (+ x 1)))
        x)
    10

如果 `ntimes` 如我们预期般的执行，这个表达式应该会对 `x`
递增五次，最后返回 `15` 。但因为宏展开刚好使用 `x` 作为迭代变量， `setf`
表达式递增那个 `x`
，而不是我们要递增的那个。一旦宏调用被展开，前述的展开式变成:

    > (let ((x 10))
        (do ((x 0 (+ x 1)))
            ((>= x 5))
          (setf x (+ x 1)))
        x)

最普遍的解法是不要使用任何可能会被捕捉的一般符号。取而代之的我们使用
gensym (8.4 小节)。因为 `read` 函数 `intern`
每个它见到的符号，所以在一个程序里，没有可能会有任何符号会 `eql`
gensym。如果我们使用 gensym 而不是 `x` 来重写 `ntimes`
的定义，至少对于变量捕捉来说，它是安全的:

    (defmacro ntimes (n &rest body)
      (let ((g (gensym)))
        `(do ((,g 0 (+ ,g 1)))
             ((>= ,g ,n))
           ,@body)))

但这个宏在另一问题上仍有疑虑: 多重求值 (multiple
evaluation)。因为第一个参数被直接插入 `do`
表达式，它会在每次迭代时被求值。当第一个参数是有副作用的表达式，这个错误非常清楚地表现出来:

    > (let ((v 10))
        (ntimes (setf v (- v 1))
          (princ ".")))
    .....
    NIL

由于 `v` 一开始是 `10` ，而 `setf`
返回其第二个参数的值，应该印出九个句点。实际上它只印出五个。

如果我们看看宏调用所展开的表达式，就可以知道为什么:

    > (let ((v 10))
        (do ((#:g1 0 (+ #:g1 1)))
            ((>= #:g1 (setf v (- v 1))))
          (princ ".")))

每次迭代我们不是把迭代变量 (gensym 通常印出前面有 `#:` 的符号)与 `9`
比较，而是与每次求值时会递减的表达式比较。这如同每次我们查看地平线时，地平线都越来越近。

避免非预期的多重求值的方法是设置一个变量，在任何迭代前将其设为有疑惑的那个表达式。这通常牵扯到另一个
gensym:

    (defmacro ntimes (n &rest body)
      (let ((g (gensym))
            (h (gensym)))
        `(let ((,h ,n))
           (do ((,g 0 (+ ,g 1)))
               ((>= ,g ,h))
             ,@body))))

终于，这是一个 `ntimes` 的正确定义。

非预期的变量捕捉与多重求值是折磨宏的主要问题，但不只有这些问题而已。有经验后，要避免这样的错误与避免更熟悉的错误一样简单，比如除以零的错误。

你的 Common Lisp
实现是一个学习更多有关宏的好地方。借由调用展开至内置宏，你可以理解它们是怎么写的。下面是大多数实现对于一个
`cond` 表达式会产生的展开式:

    > (pprint (macroexpand-1 '(cond (a b)
                                    (c d e)
                                    (t f))))
    (IF A
        B
        (IF C
            (PROGN D E)
            F))

函数 `pprint` 印出像代码一样缩排的表达式，这在检视宏展开式时特别有用。

10.6 通用化引用 (Generalized Reference)
---------------------------------------

由于一个宏调用可以直接在它出现的地方展开成代码，任何展开为 `setf`
表达式的宏调用都可以作为 `setf` 表达式的第一个参数。
举例来说，如果我们定义一个 `car` 的同义词，

    (defmacro cah (lst) `(car ,lst))

然后因为一个 `car` 调用可以是 `setf` 的第一个参数，而 `cah` 一样可以:

    > (let ((x (list 'a 'b 'c)))
        (setf (cah x) 44)
        x)
    (44 B C)

撰写一个展开成一个 `setf`
表达式的宏是另一个问题，是一个比原先看起来更为困难的问题。看起来也许你可以这样实现
`incf` ，只要

    (defmacro incf (x &optional (y 1)) ; wrong
      `(setf ,x (+ ,x ,y)))

但这是行不通的。这两个表达式不相等:

    (setf (car (push 1 lst)) (1+ (car (push 1 lst))))

    (incf (car (push 1 lst)))

如果 `lst` 是 `nil` 的话，第二个表达式会设成 `(2)`
，但第一个表达式会设成 `(1 2)` 。

Common Lisp 提供了 `define-modify-macro` 作为写出对于 `setf`
限制类别的宏的一种方法 它接受三个参数: 宏的名字，额外的参数
(隐含第一个参数 `place`)，以及产生出 `place`
新数值的函数名。所以我们可以将 `incf` 定义为

    (define-modify-macro our-incf (&optional (y 1)) +)

另一版将元素推至列表尾端的 `push` 可写成：

    (define-modify-macro append1f (val)
      (lambda (lst val) (append lst (list val))))

后者会如下工作:

    > (let ((lst '(a b c)))
        (append1f lst 'd)
        lst)
    (A B C D)

顺道一提， `push` 与 `pop` 都不能定义为 modify-macros，前者因为 `place`
不是其第一个参数，而后者因为其返回值不是更改后的对象。

10.7 示例：实用的宏函数 (Example: Macro Utilities)
--------------------------------------------------

6.4 节介绍了实用函数 (utility)的概念，一种像是构造 Lisp
的通用操作符。我们可以使用宏来定义不能写作函数的实用函数。我们已经见过几个例子:
`nil!` , `ntimes` 以及 `while`
，全部都需要写成宏，因为它们全都需要某种控制参数求值的方法。本节给出更多你可以使用宏写出的多种实用函数。图
10.2 挑选了几个实践中证实值得写的实用函数。

    (defmacro for (var start stop &body body)
      (let ((gstop (gensym)))
        `(do ((,var ,start (1+ ,var))
              (,gstop ,stop))
             ((> ,var ,gstop))
           ,@body)))

    (defmacro in (obj &rest choices)
      (let ((insym (gensym)))
        `(let ((,insym ,obj))
           (or ,@(mapcar #'(lambda (c) `(eql ,insym ,c))
                         choices)))))

    (defmacro random-choice (&rest exprs)
      `(case (random ,(length exprs))
         ,@(let ((key -1))
             (mapcar #'(lambda (expr)
                         `(,(incf key) ,expr))
                     exprs))))

    (defmacro avg (&rest args)
      `(/ (+ ,@args) ,(length args)))

    (defmacro with-gensyms (syms &body body)
      `(let ,(mapcar #'(lambda (s)
                         `(,s (gensym)))
                     syms)
         ,@body))

    (defmacro aif (test then &optional else)
      `(let ((it ,test))
         (if it ,then ,else)))

**图 10.2: 实用宏函数**

第一个 `for` ，设计上与 `while` 相似 (164 页，译注: 10.3
节)。它是给需要使用一个绑定至一个值的范围的新变量来对主体求值的循环:

    > (for x 1 8
          (princ x))
    12345678
    NIL

这比写出等效的 `do` 来得省事，

    (do ((x 1 (+ x 1)))
        ((> x 8))
      (princ x))

这非常接近实际的展开式:

    (do ((x 1 (1+ x))
         (#:g1 8))
        ((> x #:g1))
      (princ x))

宏需要引入一个额外的变量来持有标记范围 (range)结束的值。 上面在例子里的
`8` 也可是个函数调用，这样我们就不需要求值好几次。额外的变量需要是一个
gensym ，为了避免非预期的变量捕捉。

图 10.2 的第二个宏 `in` ，若其第一个参数 `eql`
任何自己其他的参数时，返回真。表达式我们可以写成:

    (in (car expr) '+ '- '*)

我们可以改写成:

    (let ((op (car expr)))
      (or (eql op '+)
          (eql op '-)
          (eql op '*)))

确实，第一个表达式展开后像是第二个，除了变量 `op` 被一个 gensym 取代了。

下一个例子 `random-choice` ，随机选取一个参数求值。在 74 页 (译注: 第 4
章的图 4.6)我们需要随机在两者之间选择。 `random-choice`
宏实现了通用的解法。一个像是这样的调用:

    (random-choice (turn-left) (turn-right))

会被展开为:

    (case (random 2)
      (0 (turn-left))
      (1 (turn-right)))

下一个宏 `with-gensyms`
主要预期用在宏主体里。它不寻常，特别是在特定应用中的宏，需要 gensym
几个变量。有了这个宏，与其

    (let ((x (gensym)) (y (gensym)) (z (gensym)))
        ...)

我们可以写成

    (with-gensyms (x y z)
        ...)

到目前为止，图 10.2
定义的宏，没有一个可以定义成函数。作为一个规则，写成宏是因为你不能将它写成函数。但这个规则有几个例外。有时候你或许想要定义一个操作符来作为宏，好让它在编译期完成它的工作。宏
`avg` 返回其参数的平均值，

    > (avg 2 4 8)
    14/3

是一个这种例子的宏。我们可以将 `avg` 写成函数，

    (defun avg (&rest args)
      (/ (apply #'+ args) (length args)))

但它会需要在执行期找出参数的数量。只要我们愿意放弃应用 `avg`
，为什么不在编译期调用 `length` 呢？

图 10.2 的最后一个宏是 `aif`
，它在此作为一个故意变量捕捉的例子。它让我们可以使用变量 `it`
来引用到一个条件式里的测试参数所返回的值。也就是说，与其写成

    (let ((val (calculate-something)))
      (if val
          (1+ val)
          0))

我们可以写成

    (aif (calculate-something)
         (1+ it)
         0)

**小心使用** ( *Use
judiciously*)，预期的变量捕捉可以是一个无价的技巧。Common Lisp
本身在多处使用它: 举例来说 `next-method-p` 与 `call-next-method`
皆依赖于变量捕捉。

像这些宏明确演示了为何要撰写替你写程序的程序。一旦你定义了 `for`
，你就不需要写整个 `do`
表达式。值得写一个宏只为了节省打字吗？非常值得。节省打字是程序设计的全部；一个编译器的目的便是替你省下使用机械语言输入程序的时间。而宏允许你将同样的优点带到特定的应用里，就像高阶语言带给程序语言一般。通过审慎的使用宏，你也许可以使你的程序比起原来大幅度地精简，并使程序更显着地容易阅读、撰写及维护。

如果仍对此怀疑，考虑看看如果你没有使用任何内置宏时，程序看起来会是怎么样。所有宏产生的展开式，你会需要用手产生。你也可以将这个问题用在另一方面。当你在撰写一个程序时，扪心自问，我需要撰写宏展开式吗？如果是的话，宏所产生的展开式就是你需要写的东西。

10.8 源自 Lisp (On Lisp)
------------------------

现在宏已经介绍过了，我们看过更多的 Lisp 是由超乎我们想像的 Lisp
写成。许多不是函数的 Common Lisp 操作符是宏，而他们全部用 Lisp
写成的。只有二十五个 Common Lisp 内置的操作符是特殊操作符。

[John Foderaro](http://www.franz.com/about/bios/jkf.lhtml) 将 Lisp
称为“可程序的程序语言。”
[λ](http://acl.readthedocs.org/en/latest/zhCN/notes-cn.html#notes-173)
通过撰写你自己的函数与宏，你将 Lisp 变成任何你想要的语言。 (我们会在 17
章看到这个可能性的图形化示范)无论你的程序适合何种形式，你确信你可以将
Lisp 塑造成适合它的语言。

宏是这个灵活性的主要成分之一。它们允许你将 Lisp
变得完全认不出来，但仍然用一种有原则且高效的方法来实作。在 Lisp
社区里，宏是个越来越感兴趣的主题。可以使用宏办到惊人之事是很清楚的，但更确信的是宏背后还有更多需要被探索。如果你想的话，可以通过你来发现。Lisp
永远将进化放在程序员手里。这是它为什么存活的原因。

Chapter 10 总结 (Summary)
-------------------------

1.  调用 `eval` 是让 Lisp
    将列表视为代码的一种方法，但这是不必要而且效率低落的。
2.  你通过叙说一个调用会展开成什么来定义一个宏。台面底下，宏只是返回表达式的函数。
3.  一个使用反引号定义的主体看起来像它会产生出的展开式 (expansion)。
4.  宏设计者需要注意变量捕捉及多重求值。宏可以通过漂亮印出
    (pretty-printing)来测试它们的展开式。
5.  多重求值是大多数展开成 `setf` 表达式的问题。
6.  宏比函数来得灵活，可以用来定义许多实用函数。你甚至可以使用变量捕捉来获得好处。
7.  Lisp
    存活的原因是它将进化交给程序员的双手。宏是使其可能的部分原因之一。

Chapter 10 练习 (Exercises)
---------------------------

1.  如果 `x` 是 `a` ， `y` 是 `b` 以及 `z` 是 `(c d)`
    ，写出反引用表达式仅包含产生下列结果之一的变量:

<!-- -->

    (a) ((C D) A Z)

    (b) (X B C D)

    (c) ((C D A) Z)

2.  使用 `cond` 来定义 `if` 。
3.  定义一个宏，接受一个数字 *n* ，伴随着一个或多个表达式，并返回第 *n*
    个表达式的值:

<!-- -->

    > (let ((n 2))
        (nth-expr n (/ 1 0) (+ 1 2) (/ 1 0)))
    3

4.  定义 `ntimes` (167 页，译注: 10.5 节)使其展开成一个
    (区域)递归函数，而不是一个 `do` 表达式。
5.  定义一个宏 `n-of` ，接受一个数字 *n* 与一个表达式，返回一个 *n*
    个渐进值:

<!-- -->

    > (let ((i 0) (n 4))
        (n-of n (incf i)))
    (1 2 3 4)

6.  定义一个宏，接受一变量列表以及一个代码主体，并确保变量在代码主体被求值后恢复
    (revert)到原本的数值。
7.  下面这个 `push` 的定义哪里错误？

<!-- -->

    (defmacro push (obj lst)
      `(setf ,lst (cons ,obj ,lst)))

    举出一个不会与实际 push 做一样事情的函数调用例子。

8.  定义一个将其参数翻倍的宏:

<!-- -->

    > (let ((x 1))
        (double x)
        x)
    2

**脚注**

[^1]: 要真的复制一个 Lisp 的话， `eval` 会需要接受第二个参数 (这里的
    `env`) 来表示词法环境 (lexical enviroment)。这个模型的 `eval`
    是不正确的，因为它在对参数求值前就取出函数，然而 Common Lisp
    故意没有特别指出这两个操作的顺序。
