附录 A：调试
============

这个附录演示了如何调试 Lisp 程序，并给出你可能会遇到的常见错误。

中断循环 (Breakloop)
--------------------

如果你要求 Lisp
做些它不能做的事，求值过程会被一个错误讯息中断，而你会发现你位于一个称为中断循环的地方。中断循环工作的方式取决于不同的实现，但通常它至少会显示三件事：一个错误信息，一组选项，以及一个特别的提示符。

在中断循环里，你也可以像在顶层那样给表达式求值。在中断循环里，你或许能够找出错误的起因，甚至是修正它，并继续你程序的求值过程。然而，在一个中断循环里，你想做的最常见的事是跳出去。多数的错误起因于打错字或是小疏忽，所以通常你只会想终止程序并返回顶层。在下面这个假定的实现里，我们输入
`:abort` 来回到顶层。

    > (/ 1 0)
    Error: Division by zero.
           Options: :abort, :backtrace
    >> :abort
    >

在这些情况里，实际上的输入取决于实现。

当你在中断循环里，如果一个错误发生的话，你会到另一个中断循环。多数的
Lisp
会指出你是在第几层的中断循环，要嘛通过印出多个提示符，不然就是在提示符前印出数字：

    >> (/ 2 0)
    Error: Division by zero.
           Options: :abort, :backtrace, :previous
    >>>

现在我们位于两层深的中断循环。此时我们可以选择回到前一个中断循环，或是直接返回顶层。

追踪与回溯 (Traces and Backtraces)
----------------------------------

当你的程序不如你预期的那样工作时，有时候第一件该解决的事情是，它在做什么？如果你输入
`(trace foo)` ，则 Lisp 会在每次调用或返回 `foo`
时显示一个信息，显示传给 `foo` 的参数，或是 `foo`
返回的值。你可以追踪任何自己定义的 (user-defined)函数。

一个追踪通常会根据调用树来缩进。在一个做遍历的函数，像下面这个函数，它给一个树的每一个非空元素加上
1，

    (defun tree1+ (tr)
      (cond ((null tr) nil)
            ((atom tr) (1+ tr))
            (t (cons (treel+ (car tr))
                     (treel+ (cdr tr))))))

一个树的形状会因此反映出它被遍历时的数据结构：

    > (trace tree1+)
    (tree1+)
    > (tree1+ '((1 . 3) 5 . 7))
    1 Enter TREE1+ ((1 . 3) 5 . 7)
      2 Enter TREE1+ (1.3)
        3 Enter TREE1+ 1
        3 Exit TREE1+ 2
        3 Enter TREE1+ 3
        3 Exit TREE1+ 4
      2 Exit TREE1+ (2 . 4)
      2 Enter TREE1+ (5 . 7)
        3 Enter TREE1+ 5
        3 Exit TREE1+ 6
        3 Enter TREE1+ 7
        3 Exit TREE1+ 8
      2 Exit TREE1+ (6 . 8)
    1 Exit TREE1+ ((2 . 4) 6 . 8)
    ((2 . 4) 6 . 8)

要关掉 `foo` 的追踪，输入 `(untrace foo)`
；要关掉所有正在追踪的函数，只要输入 `(untrace)` 就好。

一个更灵活的追踪办法是在你的代码里插入诊断性的打印语句。如果已经知道结果了，这个经典的方法大概会与复杂的调适工具一样被使用数十次。这也是为什么可以互动地重定义函数式多么有用的原因。

一个回溯
(*backtrace*)是一个当前存在栈的调用的列表，当一个错误中止求值时，会由一个中断循环生成此列表。如果追踪像是"让我看看你在做什么"，一个回溯像是询问"我们是怎么到达这里的？"
在某方面上，追踪与回溯是互补的。一个追踪会显示在一个程序的调用树里，选定函数的调用。一个回溯会显示在一个程序部分的调用树里，所有函数的调用（路径为从顶层调用到发生错误的地方）。

在一个典型的实现里，我们可通过在中断循环里输入 `:backtrace`
来获得一个回溯，看起来可能像下面这样：

    > (tree1+ ' ( ( 1 . 3) 5 . A))
    Error: A is not a valid argument to 1+.
           Options: :abort, :backtrace
    » :backtrace
    (1+ A)
    (TREE1+ A)
    (TREE1+ (5 . A))
    (TREE1+ ((1 . 3) 5 . A))

出现在回溯里的臭虫较容易被发现。你可以仅往回检查调用链，直到你找到第一个不该发生的事情。另一个函数式编程
(2.12
节)的好处是所有的臭虫都会在回溯里出现。在纯函数式代码里，每一个可能出错的调用，在错误发生时，一定会在栈出现。

一个回溯每个实现所提供的信息量都不同。某些实现会完整显示一个所有待调用的历史，并显示参数。其他实现可能仅显示调用历史。一般来说，追踪与回溯解释型的代码会得到较多的信息，这也是为什么你要在确定你的程序可以工作之后，再来编译。

传统上我们在解释器里调试代码，且只在工作的情况下才编译。但这个观点也是可以改变的：至少有两个
Common Lisp 实现没有包含解释器。

当什么事都没发生时 (When Noting Happens)
----------------------------------------

不是所有的 bug 都会打断求值过程。另一个常见并可能更危险的情况是，当 Lisp
好像不鸟你一样。通常这是程序进入无穷循环的徵兆。

如果你怀疑你进入了无穷循环，解决方法是中止执行，并跳出中断循环。

如果循环是用迭代写成的代码，Lisp
会开心地执行到天荒地老。但若是用递归写成的代码（没有做尾递归优化），你最终会获得一个信息，信息说
Lisp 把栈的空间给用光了：

    > (defun blow-stack () (1+ (blow-stack)))
    BLOW-STACK
    > (blow-stack)
    Error: Stack Overflow

在这两个情况里，如果你怀疑进入了无穷循环，解决办法是中断执行，并跳出由于中断所产生的中断循环。

有时候程序在处理一个非常庞大的问题时，就算没有进入无穷循环，也会把栈的空间用光。虽然这很少见。通常把栈空间用光是编程错误的徵兆。

递归函数最常见的错误是忘记了基本用例 (base
case)。用英语来描述递归，通常会忽略基本用例。不严谨地说，我们可能说“obj
是列表的成员，如果它是列表的第一个元素，或是剩余列表的成员”
严格上来讲，应该添加一句“若列表为空，则 obj
不是列表的成员”。不然我们描述的就是个无穷递归了。

在 Common Lisp 里，如果给入 `nil` 作为参数， `car` 与 `cdr` 皆返回 `nil`
：

    > (car nil)
    NIL
    > (cdr nil)
    NIL

所以若我们在 `member` 函数里忽略了基本用例：

    (defun our-member (obj lst)
      (if (eql (car lst) obj)
          lst
          (our-member obj (cdr lst))))

要是我们找的对象不在列表里的话，则会陷入无穷循环。当我们到达列表底端而无所获时，递归调用会等价于：

    (our-member obj nil)

在正确的定义中（第十六页「译注: 2.7
节」），基本用例在此时会停止递归，并返回 `nil`
。但在上面错误的定义里，函数愚昧地寻找 `nil` 的 `car` ，是 `nil` ，并将
`nil` 拿去跟我们寻找的对象比较。除非我们要找的对象刚好是 `nil`
，不然函数会继续在 `nil` 的 `cdr` 里寻找，刚好也是 `nil` ──
整个过程又重来了。

如果一个无穷循环的起因不是那么直观，可能可以通过看看追踪或回溯来诊断出来。无穷循环有两种。简单发现的那种是依赖程序结构的那种。一个追踪或回溯会即刻演示出，我们的
`our-member` 究竟哪里出错了。

比较难发现的那种，是因为数据结构有缺陷才发生的无穷循环。如果你无意中创建了环状结构（见
199页「12.3 节」，遍历结构的代码可能会掉入无穷循环里。这些 bug
很难发现，因为不在后面不会发生，看起来像没有错误的代码一样。最佳的解决办法是预防，如同
199
页所描述的：避免使用破坏性操作，直到程序已经正常工作，且你已准备好要调优代码来获得效率。

如果 Lisp
有不鸟你的倾向，也有可能是等待你完成输入什么。在多数系统里，按下回车是没有效果的，直到你输入了一个完整的表达式。这个方法的好事是它允许你输入多行的表达式。坏事是如果你无意中少了一个闭括号，或是一个闭引号，Lisp
会一直等你，直到你真正完成输入完整的表达式：

    > (format t "for example ~A~% 'this)

这里我们在控制字符串的最后忽略了闭引号。在此时按下回车是没用的，因为
Lisp 认为我们还在输入一个字符串。

在某些实现里，你可以回到上一行，并插入闭引号。在不允许你回到前行的系统，最佳办法通常是中断执行，并从中断循环回到顶层。

没有值或未绑定 (No Value/Unbound)
---------------------------------

一个你最常听到 Lisp
的抱怨是一个符号没有值或未绑定。数种不同的问题都用这种方式呈现。

局部变量，如 `let` 与 `defun`
设置的那些，只在创建它们的表达式主体里合法。所以要是我们试着在
创建变量的 `let` 外部引用它，

    > (progn
        (let ((x 10))
          (format t "Here x = ~A. ~%" x))
        (format t "But now it's gone...~%")
        x)
    Here x = 10.
    But now it's gone...
    Error: X has no value.

我们获得一个错误。当 Lisp
抱怨某些东西没有值或未绑定时，它的意思通常是你无意间引用了一个不存在的变量。因为没有叫做
`x` 的局部变量，Lisp
假定我们要引用一个有着这个名字的全局变量或常量。错误会发生是因为当 Lisp
试着要查找它的值的时候，却发现根本没有给值。打错变量的名字通常会给出同样的结果。

一个类似的问题发生在我们无意间将函数引用成变量。举例来说：

    > defun foo (x) (+ x 1))
    Error: DEFUN has no value

这在第一次发生时可能会感到疑惑： `defun`
怎么可能会没有值？问题的症结点在于我们忽略了最初的左括号，导致 Lisp
把符号 `defun` 解读错误，将它视为一个全局变量的引用。

有可能你真的忘记初始化某个全局变量。如果你没有给 `defvar`
第二个参数，你的全局变量会被宣告出来，但没有初始化；这可能是问题的根源。

意料之外的 Nil (Unexpected Nils)
--------------------------------

当函数抱怨传入 `nil`
作为参数时，通常是程序先前出错的徵兆。数个内置操作符返回 `nil`
来指出失败。但由于 `nil` 是一个合法的 Lisp
对象，问题可能之后才发生，在程序某部分试着要使用这个信以为真的返回值时。

举例来说，返回一个月有多少天的函数有一个 bug；假设我们忘记十月份了：

    (defun month-length (mon)
      (case mon
        ((jan mar may jul aug dec) 31)
        ((apr jun sept nov) 30)
        (feb (if (leap-year) 29 28))))

如果有另一个函数，企图想计算出一个月当中有几个礼拜，

    (defun month-weeks (mon) (/ (month-length mon) 7.0))

则会发生下面的情形：

    > (month-weeks 'oct)
    Error: NIL is not a valud argument to /.

问题发生的原因是因为 `month-length` 在 `case` 找不到匹配
。当这个情形发生时， `case` 返回 `nil` 。然后 `month-weeks`
，认为获得了一个数字，将值传给 `/` ，`/` 就抱怨了。

在这里最起码 bug 与 bug 的临床表现是挨着发生的。这样的 bug
在它们相距很远时很难找到。要避免这个可能性，某些 Lisp 方言让跑完 `case`
或 `cond` 又没匹配的情形，产生一个错误。在 Common Lisp
里，在这种情况里可以做的是使用 `ecase` ，如 14.6 节所描述的。

重新命名 (Renaming)
-------------------

在某些场合里（但不是全部场合），有一种特别狡猾的 bug
，起因于重新命名函数或变量，。举例来说，假设我们定义下列（低效的）
函数来找出双重嵌套列表的深度：

    (defun depth (x)
      (if (atom x)
          1
          (1+ (apply #'max (mapcar #'depth x)))))

测试函数时，我们发现它给我们错误的答案（应该是 1）：

    > (depth '((a)))
    3

起初的 `1` 应该是 `0`
才对。如果我们修好这个错误，并给这个函数一个较不模糊的名称：

    (defun nesting-depth (x)
      (if (atom x)
          0
          (1+ (apply #'max (mapcar #'depth x)))))

当我们再测试上面的例子，它返回同样的结果：

    > (nesting-depth '((a)))
    3

我们不是修好这个函数了吗？没错，但答案不是来自我们修好的代码。我们忘记也改掉递归调用中的名称。在递归用例里，我们的新函数仍调用先前的
`depth` ，这当然是不对的。

作为选择性参数的关键字 (Keywords as Optional Parameters)
--------------------------------------------------------

若函数同时接受关键字与选择性参数，这通常是个错误，无心地提供了关键字作为选择性参数。举例来说，函数
`read-from-string` 有着下列的参数列表：

    (read-from-string string &optional eof-error eof-value
                             &key start end preserve-whitespace)

这样一个函数你需要依序提供值，给所有的选择性参数，再来才是关键字参数。如果你忘记了选择性参数，看看下面这个例子，

    > (read-from-string "abcd" :start 2)
    ABCD
    4

则 `:start` 与 `2` 会成为前两个选择性参数的值。若我们想要 `read`
从第二个字符开始读取，我们应该这么说：

    > (read-from-string "abcd" nil nil :start 2)
    CD
    4

错误声明 (Misdeclarations)
--------------------------

第十三章解释了如何给变量及数据结构做类型声明。通过给变量做类型声明，你保证变量只会包含某种类型的值。当产生代码时，Lisp
编译器会依赖这个假定。举例来说，这个函数的两个参数都声明为
`double-floats` ，

    (defun df* (a b)
      (declare (double-float a b))
      (* a b))

因此编译器在产生代码时，被授权直接将浮点乘法直接硬连接
(hard-wire)到代码里。

如果调用 `df*`
的参数不是声明的类型时，可能会捕捉一个错误，或单纯地返回垃圾。在某个实现里，如果我们传入两个定长数，我们获得一个硬体中断：

    > (df* 2 3)
    Error: Interrupt.

如果获得这样严重的错误，通常是由于数值不是先前声明的类型。

警告 (Warnings)
---------------

有些时候 Lisp
会抱怨一下，但不会中断求值过程。许多这样的警告是错误的警钟。一种最常见的可能是由编译器所产生的，关于未宣告或未使用的变量。举例来说，在
66 页「译注: 6.4 节」， `map-int` 的第二个调用，有一个 `x`
变量没有使用到。如果想要编译器在每次编译程序时，停止通知你这些事，使用一个忽略声明：

    (map-int #'(lambda (x)
                 (declare (ignore x))
                 (random 100))
             10)
