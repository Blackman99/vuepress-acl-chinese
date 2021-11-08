第五章：控制流
==============

2.2 节介绍过 Common Lisp
的求值规则，现在你应该很熟悉了。本章的操作符都有一个共同点，就是它们都违反了求值规则。这些操作符让你决定在程序当中何时要求值。如果普通的函数调用是
Lisp 程序的树叶的话，那这些操作符就是连结树叶的树枝。

5.1 区块 (Blocks)
-----------------

Common Lisp 有三个构造区块（block）的基本操作符： `progn` 、 `block`
以及 `tagbody` 。我们已经看过 `progn` 了。在 `progn`
主体中的表达式会依序求值，并返回最后一个表达式的值：

    > (progn
        (format t "a")
        (format t "b")
        (+ 11 12))
    ab
    23

由于只返回最后一个表达式的值，代表着使用 `progn`
（或任何区块）涵盖了副作用。

一个 `block` 像是带有名字及紧急出口的 `progn`
。第一个实参应为符号。这成为了区块的名字。在主体中的任何地方，可以停止求值，并通过使用
`return-from` 指定区块的名字，来立即返回数值：

    > (block head
        (format t "Here we go.")
        (return-from head 'idea)
        (format t "We'll never see this."))
    Here we go.
    IDEA

调用 `return-from`
允许你的程序，从代码的任何地方，突然但优雅地退出。第二个传给
`return-from` 的实参，用来作为以第一个实参为名的区块的返回值。在
`return-from` 之后的表达式不会被求值。

也有一个 `return` 宏，它把传入的参数当做封闭区块 `nil` 的返回值：

    > (block nil
        (return 27))
    27

许多接受一个表达式主体的 Common Lisp 操作符，皆隐含在一个叫做 `nil`
的区块里。比如，所有由 `do` 构造的迭代函数：

    > (dolist (x '(a b c d e))
        (format t "~A " x)
        (if (eql x 'c)
            (return 'done)))
    A B C
    DONE

使用 `defun` 定义的函数主体，都隐含在一个与函数同名的区块，所以你可以：

    (defun foo ()
      (return-from foo 27))

在一个显式或隐式的 `block` 外，不论是 `return-from` 或 `return`
都不会工作。

使用 `return-from` ，我们可以写出一个更好的 `read-integer` 版本：

    (defun read-integer (str)
      (let ((accum 0))
        (dotimes (pos (length str))
          (let ((i (digit-char-p (char str pos))))
            (if i
                (setf accum (+ (* accum 10) i))
                (return-from read-integer nil))))
        accum))

68
页的版本在构造整数之前，需检查所有的字符。现在两个步骤可以结合，因为如果遇到非数字的字符时，我们可以舍弃计算结果。出现在主体的原子（atom）被解读为标签（labels)；把这样的标签传给
`go`
，会把控制权交给标签后的表达式。以下是一个非常丑的程序片段，用来印出一至十的数字：

    > (tagbody
        (setf x 0)
        top
          (setf x (+ x 1))
          (format t "~A " x)
          (if (< x 10) (go top)))
    1 2 3 4 5 6 7 8 9 10
    NIL

这个操作符主要用来实现其它的操作符，不是一般会用到的操作符。大多数迭代操作符都隐含在一个
`tagbody` ，所以是可能可以在主体里（虽然很少想要）使用标签及 `go` 。

如何决定要使用哪一种区块建构子呢（block
construct）？几乎任何时候，你会使用 `progn`
。如果你想要突然退出的话，使用 `block`
来取代。多数程序员永远不会显式地使用 `tagbody` 。

5.2 语境 (Context)
------------------

另一个我们用来区分表达式的操作符是 `let`
。它接受一个代码主体，但允许我们在主体内设置新变量：

    > (let ((x 7)
            (y 2))
        (format t "Number")
        (+ x y))
    Number
    9

一个像是 `let` 的操作符，创造出一个新的词法语境（lexical
context）。在这个语境里有两个新变量，然而在外部语境的变量也因此变得不可视了。

概念上说，一个 `let` 表达式等同于函数调用。在 2.14
节证明过，函数可以用名字来引用，也可以通过使用一个 lambda
表达式从字面上来引用。由于 lambda
表达式是函数的名字，我们可以像使用函数名那样，把 lambda
表达式作为函数调用的第一个实参：

    > ((lambda (x) (+ x 1)) 3)
    4

前述的 `let` 表达式，实际上等同于：

    ((lambda (x y)
       (format t "Number")
       (+ x y))
     7
     2)

如果有关于 `let` 的任何问题，应该是如何把责任交给 `lambda`
，因为进入一个 `let` 等同于执行一个函数调用。

这个模型清楚的告诉我们，由 `let` 创造的变量的值，不能依赖其它由同一个
`let` 所创造的变量。举例来说，如果我们试着：

    (let ((x 2)
          (y (+ x 1)))
      (+ x y))

在 `(+ x 1)` 中的 `x` 不是前一行所设置的值，因为整个表达式等同于：

    ((lambda (x y) (+ x y)) 2
                            (+ x 1))

这里明显看到 `(+ x 1)` 作为实参传给函数，不能引用函数内的形参 `x` 。

所以如果你真的想要新变量的值，依赖同一个表达式所设立的另一个变量？在这个情况下，使用一个变形版本
`let*` ：

    > (let* ((x 1)
             (y (+ x 1)))
        (+ x y))
    3

一个 `let*` 功能上等同于一系列嵌套的 `let` 。这个特别的例子等同于：

    (let ((x 1))
      (let ((y (+ x 1)))
        (+ x y)))

`let` 与 `let*` 将变量初始值都设为 `nil` 。`nil`
为初始值的变量，不需要依附在列表内:

    > (let (x y)
        (list x y))
    (NIL NIL)

`destructuring-bind` 宏是通用化的 `let` 。其接受单一变量，一个模式
(pattern) ── 一个或多个变量所构成的树 ──
并将它们与某个实际的树所对应的部份做绑定。举例来说：

    > (destructuring-bind (w (x y) . z) '(a (b c) d e)
        (list w x y z))
    (A B C (D E))

若给定的树（第二个实参）没有与模式匹配（第一个参数）时，会产生错误。

5.3 条件 (Conditionals)
-----------------------

最简单的条件式是 `if` ；其余的条件式都是基于 `if`
所构造的。第二简单的条件式是 `when` ，它接受一个测试表达式（test
expression）与一个代码主体。若测试表达式求值返回真时，则对主体求值。所以

    (when (oddp that)
      (format t "Hmm, that's odd.")
      (+ that 1))

等同于

    (if (oddp that)
        (progn
          (format t "Hmm, that's odd.")
          (+ that 1)))

`when` 的相反是 `unless`
；它接受相同的实参，但仅在测试表达式返回假时，才对主体求值。

所有条件式的母体 (从正反两面看) 是 `cond` ， `cond`
有两个新的优点：允许多个条件判断，与每个条件相关的代码隐含在 `progn`
里。 `cond` 预期在我们需要使用嵌套 `if` 的情况下使用。 举例来说，这个伪
member 函数

    (defun our-member (obj lst)
      (if (atom lst)
          nil
          (if (eql (car lst) obj)
              lst
              (our-member obj (cdr lst)))))

也可以定义成：

    (defun our-member (obj lst)
      (cond ((atom lst) nil)
            ((eql (car lst) obj) lst)
            (t (our-member obj (cdr lst)))))

事实上，Common Lisp 实现大概会把 `cond` 翻译成 `if` 的形式。

总得来说呢， `cond`
接受零个或多个实参。每一个实参必须是一个具有条件式，伴随着零个或多个表达式的列表。当
`cond`
表达式被求值时，测试条件式依序求值，直到某个测试条件式返回真才停止。当返回真时，与其相关联的表达式会被依序求值，而最后一个返回的数值，会作为
`cond` 的返回值。如果符合的条件式之后没有表达式的话：

    > (cond (99))
    99

则会返回条件式的值。

由于 `cond` 子句的 `t`
条件永远成立，通常我们把它放在最后，作为缺省的条件式。如果没有子句符合时，则
`cond` 返回 `nil` ，但利用 `nil` 作为返回值是一种很差的风格
(这种问题可能发生的例子，请看 292 页)。译注: **Appendix A, unexpected
nil** 小节。

当你想要把一个数值与一系列的常量比较时，有 `case` 可以用。我们可以使用
`case` 来定义一个函数，返回每个月份中的天数：

    (defun month-length (mon)
      (case mon
        ((jan mar may jul aug oct dec) 31)
        ((apr jun sept nov) 30)
        (feb (if (leap-year) 29 28))
        (otherwise "unknown month")))

一个 `case`
表达式由一个实参开始，此实参会被拿来与每个子句的键值做比较。接着是零个或多个子句，每个子句由一个或一串键值开始，跟随着零个或多个表达式。键值被视为常量；它们不会被求值。第一个参数的值被拿来与子句中的键值做比较
(使用 `eql`
)。如果匹配时，子句剩余的表达式会被求值，并将最后一个求值作为 `case`
的返回值。

缺省子句的键值可以是 `t` 或 `otherwise`
。如果没有子句符合时，或是子句只包含键值时，

    > (case 99 (99))
    NIL

则 `case` 返回 `nil` 。

`typecase` 宏与 `case` 相似，除了每个子句中的键值应为类型修饰符 (type
specifiers)，以及第一个实参与键值比较的函数使用 `typep` 而不是 `eql`
(一个 `typecase` 的例子在 107 页)。 **译注: 6.5 小节。**

5.4 迭代 (Iteration)
--------------------

最基本的迭代操作符是 `do` ，在 2.13 小节介绍过。由于 `do` 包含了隐式的
`block` 及 `tagbody` ，我们现在知道是可以在 `do` 主体内使用 `return` 、
`return-from` 以及 `go` 。

2.13 节提到 `do`
的第一个参数必须是说明变量规格的列表，列表可以是如下形式：

    (variable  initial  update)

`initial` 与 `update` 形式是选择性的。若 `update`
形式忽略时，每次迭代时不会更新变量。若 `initial`
形式也忽略时，变量会使用 `nil` 来初始化。

在 23 页的例子中（译注: 2.13 节），

    (defun show-squares (start end)
      (do ((i start (+ i 1)))
          ((> i end) 'done)
        (format t "~A ~A~%" i (* i i))))

`update` 形式引用到由 `do` 所创造的变量。一般都是这么用。如果一个 `do`
的 `update` 形式，没有至少引用到一个 `do` 创建的变量时，反而很奇怪。

当同时更新超过一个变量时，问题来了，如果一个 `update`
形式，引用到一个拥有自己的 `update`
形式的变量时，它会被更新呢？或是获得前一次迭代的值？使用 `do`
的话，它获得后者的值：

    > (let ((x 'a))
        (do ((x 1 (+ x 1))
             (y x x))
            ((> x 5))
          (format t "(~A ~A)  " x y)))
    (1 A)  (2 1)  (3 2)  (4 3)  (5 4)
    NIL

每一次迭代时， `x` 获得先前的值，加上一； `y` 也获得 `x` 的前一次数值。

但也有一个 `do*` ，它有着和 `let` 与 `let*` 一样的关系。任何 `initial`
或 `update` 形式可以参照到前一个子句的变量，并会获得当下的值：

    > (do* ((x 1 (+ x 1))
          (y x x))
         ((> x 5))
      (format t "(~A ~A) " x y))
    (1 1) (2 2) (3 3) (4 4) (5 5)
    NIL

除了 `do` 与 `do*`
之外，也有几个特别用途的迭代操作符。要迭代一个列表的元素，我们可以使用
`dolist` :

    > (dolist (x '(a b c d) 'done)
        (format t "~A " x))
    A B C D
    DONE

当迭代结束时，初始列表内的第三个表达式 (译注: `done` ) ，会被求值并作为
`dolist` 的返回值。缺省是 `nil` 。

有着同样的精神的是 `dotimes` ，给定某个 `n` ，将会从整数 `0` ，迭代至
`n-1` :

    (dotimes (x 5 x)
      (format t "~A " x))
    0 1 2 3 4
    5

`dolist` 与
`dotimes` 初始列表的第三个表达式皆可省略，省略时为`nil`。注意该表达式可引用到迭代过程中的变量。  （译注：第三个表达式即上例之`x`，可以省略，省略时`dotimes`表达式的返回值为`nil`。）  
::: tip do 的重点 (THE POINT OF do)
在 “The Evolution of Lisp” 里，Steele 与 Garbriel 陈述了 do 的重点，   表达的实在太好了，值得整个在这里引用过来：    撇开争论语法不谈，有件事要说明的是，在任何一个编程语言中，一个循环若一次只能更新一个变量是毫无用处的。   
几乎在任何情况下，会有一个变量用来产生下个值，而另一个变量用来累积结果。如果循环语法只能产生变量，   那么累积结果就得借由赋值语句来“手动”实现…或有其他的副作用。具有多变量的 do 循环，体现了产生与累积的本质对称性，允许可以无副作用地表达迭代过程：    
       
    (defun factorial (n)         
        (do ((j n (- j 1))              
        (f 1 (* j f)))           
        ((= j 0) f)))    
:::

当然在 step 形式里实现所有的实际工作，一个没有主体的 do 循环形式是较不寻常的。  
函数`mapc`和`mapcar`很像，但不会`cons`一个新列表作为返回值，所以使用的唯一理由是为了副作用。
它们比`dolist`来得灵活，因为可以同时遍历多个列表： 

    > (mapc #'(lambda (x y)             
        (format t "~A ~A  " x y))         
            '(hip flip slip)         
            '(hop flop slop))   
    HIP HOP  FLIP FLOP  SLIP SLOP   (HIP FLIP SLIP)  
    
总是返回`mapc`的第二个参数。 

5.5 多值 (Multiple Values) 
----------------------------

曾有人这么说，为了要强调函数式编程的重要性，每个 Lisp 表达式都返回一个值。  
现在事情不是这么简单了；在 Common Lisp 里，一个表达式可以返回零个或多个数值。最多可以返回几个值取决于各家实现，但至少可以返回 19 个值。  多值允许一个函数返回多件事情的计算结果，而不用构造一个特定的结构。
举例来说，内置的`get-decoded-time`返回 9 个数值来表示现在的时间：秒，分，时，日期，月，年，天，以及另外两个数值。  
多值也使得查询函数可以分辨出`nil`与查询失败的情况。这也是为什么`gethash`返回两个值。因为它使用第二个数值来指出成功还是失败，我们可以在哈希表里储存`nil`，就像我们可以储存别的数值那样。`values`函数返回多个数值。它一个不少地返回你作为数值所传入的实参：  

    > (values 'a nil (+ 2 4))   
    A   NIL   6 

如果一个`values`表达式，是函数主体最后求值的表达式，它所返回的数值变成函数的返回值。多值可以原封不地通过任何数量的返回来传递：  

    > ((lambda () ((lambda () (values 1 2)))))   
    1   2  
    
然而若只预期一个返回值时，第一个之外的值会被舍弃：  
    
    > (let ((x (values 1 2))) 
        x)   
    1  

通过不带实参使用`values`，是可能不返回值的。在这个情况下，预期一个返回值的话，会获得`nil`:  

    > (values)   
    > (let ((x (values)))       
        x)   
    NIL  

要接收多个数值，我们使用`multiple-value-bind`:  

    > (multiple-value-bind (x y z) (values 1 2 3)       
        (list x y z))   
    (1 2 3)    
    
    > (multiple-value-bind (x y z) 
        (values 1 2)       
        (list x y z))   
    (1 2 NIL)  

如果变量的数量大于数值的数量，剩余的变量会是`nil`。如果数值的数量大于变量的数量，多余的值会被舍弃。所以只想印出时间我们可以这么写: 
    
    > (multiple-value-bind (s m h) 
        (get-decoded-time)       
        (format t "~A:~A:~A" h m s))   
    "4:32:13"  
    
你可以借由`multiple-value-call`将多值作为实参传给第二个函数：
    
    > (multiple-value-call #'+ (values 1 2 3))   
    6  

还有一个函数是`multiple-value-list`: 
    
    > (multiple-value-list (values 'a 'b 'c))   
    (A B C)  
    
看起来像是使用`\#'list`作为第一个参数的来调用`multiple-value-call`。  

5.6 中止 (Aborts) 
----------------------------
 
你可以使用`return`在任何时候离开一个`block`。有时候我们想要做更极端的事，在数个函数调用里将控制权转移回来。要达成这件事，我们使用`catch`与`throw`。一个`catch`表达式接受一个标签（tag），标签可以是任何类型的对象，伴随着一个表达式主体：  
    
    (defun super ()     
        (catch 'abort       
            (sub)       
            (format t "We'll never see this.")))    
    (defun sub ()     
    (throw 'abort 99))  
    
表达式依序求值，就像它们是在`progn`里一样。在这段代码里的任何地方，一个带有特定标签的`throw`会导致`catch`表达式直接返回：  

    > (super)   
    99  
    
一个带有给定标签的`throw`，为了要到达匹配标签的`catch`，会将控制权转移 (因此杀掉进程)给任何有标签的`catch`。如果没有一个`catch`符合欲匹配的标签时，`throw`会产生一个错误。  
调用`error`同时中断了执行，本来会将控制权转移到调用树（calling tree）的更高点，取而代之的是，它将控制权转移给 Lisp 错误处理器（error handler）。通常会导致调用一个中断循环（break loop）。以下是一个假定的 Common Lisp 实现可能会发生的事情：  

    > (progn       
        (error "Oops!")       
        (format t "After the error."))   
    Error: Oops!          
    Options: :abort, :backtrace   
    >>  
    
> 译注：2 个`>>`显示进入中断循环了。  关于错误与状态的更多讯息，参见 14.6 小节以及附录 A。  

有时候你想要防止代码被`throw`与`error`打断。借由使用`unwind-protect`，可以确保像是前述的中断，不会让你的程序停在不一致的状态。一个`unwind-protect`接受任何数量的实参，并返回第一个实参的值。然而即便是第一个实参的求值被打断时，剩下的表达式仍会被求值：  

    > (setf x 1)   
    1   
    > (catch 'abort       
        (unwind-protect         
        (throw 'abort 99)         
        (setf x 2)))   
    99   
    > x   
    2  
    
在这里，即便`throw`将控制权交回监测的`catch`，`unwind-protect`确保控制权移交时，第二个表达式有被求值。无论何时，一个确切的动作要伴随着某种清理或重置时，`unwind-protect`可能会派上用场。在 121 页提到了一个例子。  

5.7 示例：日期运算 (Example: Date Arithmetic) 
----------------------------

在某些应用里，能够做日期的加减是很有用的 ── 举例来说，能够算出从 1997 年 12 月 17 日，六十天之后是 1998 年 2 月 15 日。在这个小节里，我们会编写一个实用的工具来做日期运算。我们会将日期转成整数，起始点设置在 2000 年 1 月 1 日。我们会使用内置的`+`与`-`函数来处理这些数字，而当我们转换完毕时，再将结果转回日期。  
要将日期转成数字，我们需要从日期的单位中，算出总天数有多少。举例来说，2004 年 11 月 13 日的天数总和，是从起始点至 2004 年有多少天，加上从 2004 年到 2004 年 11 月有多少天，再加上 13 天。  
有一个我们会需要的东西是，一张列出非润年每月份有多少天的表格。我们可以使用 Lisp 来推敲出这个表格的内容。我们从列出每月份的长度开始：  

    > (setf mon '(31 28 31 30 31 30 31 31 30 31 30 31))   
    (31 28 31 30 31 30 31 31 30 31 30 31)  
    
我们可以通过应用`+`函数至这个列表来测试总长度：  

    > (apply #'+ mon)   
    365  
        
现在如果我们反转这个列表并使用`maplist`来应用`+`函数至每下一个`cdr`上，我们可以获得从每个月份开始所累积的天数：

    > (setf nom (reverse mon))   
    (31 30 31 30 31 31 30 31 30 31 28 31)   
    > (setf sums (maplist #'(lambda (x)                             
        (apply #'+ x))                         
        nom))   
    (365 334 304 273 243 212 181 151 120 90 59 31)  

这些数字体现了从二月一号开始已经过了 31 天，从三月一号开始已经过了 59 天……等等。  
我们刚刚建立的这个列表，可以转换成一个向量，见图 5.1，转换日期至整数的代码。

    (defconstant month     
        #(0 31 59 90 120 151 181 212 243 273 304 334 365))    
    (defconstant yzero 2000)    
    (defun leap? (y)     
        (and (zerop (mod y 4))          
        (or (zerop (mod y 400))              
        (not (zerop (mod y 100))))))    
    (defun date->num (d m y)     
        (+ (- d 1) (month-num m y) (year-num y)))    
    (defun month-num (m y)     
        (+ (svref month (- m 1))        
        (if (and (> m 2) (leap? y)) 1 0)))    
    (defun year-num (y)     
        (let ((d 0))       
        (if (>= y yzero)           
        (dotimes (i (- y yzero) d)             
        (incf d (year-days (+ yzero i))))           
        (dotimes (i (- yzero y) (- d))             
        (incf d (year-days (+ y i)))))))    
    (defun year-days (y) (if (leap? y) 366 365))  
            
**图 5.1 日期运算：转换日期至数字** 

典型 Lisp 程序的生命周期有四个阶段：先写好，然后读入，接着编译，最后执行。有件 Lisp 非常独特的事情之一是，在这四个阶段时， Lisp 一直都在那里。可以在你的程序编译 (参见 10.2 小节)或读入时 (参见 14.3 小节) 来调用 Lisp。我们推导出`month`的过程演示了，如何在撰写一个程序时使用 Lisp。  
效率通常只跟第四个阶段有关系，运行期（run-time）。在前三个阶段，你可以随意的使用列表拥有的威力与灵活性，而不需要担心效率。  
若你使用图 5.1 的代码来造一个时光机器（time machine），当你抵达时，人们大概会不同意你的日期。即使是相对近的现在，欧洲的日期也曾有过偏移，因为人们会获得更精准的每年有多长的概念。在说英语的国家，最后一次的不连续性出现在 1752 年，日期从 9 月 2 日跳到 9 月 14 日。  每年有几天取决于该年是否是润年。如果该年可以被四整除，我们说该年是润年，除非该年可以被 100 整除，则该年非润年 ── 而要是它可以被 400 整除，则又是润年。所以 1904 年是润年，1900 年不是，而 1600 年是。  要决定某个数是否可以被另个数整除，我们使用函数`mod`，返回相除后的余数：  

    > (mod 23 5)   
    3   
    > (mod 25 5)   
    0  
    
如果第一个实参除以第二个实参的余数为 0，则第一个实参是可以被第二个实参整除的。函数`leap?`使用了这个方法，来决定它的实参是否是一个润年：  

    > (mapcar #'leap? '(1904 1900 1600))   
    (T NIL T)  
    
我们用来转换日期至整数的函数是`date->num`。它返回日期中每个单位的天数总和。要找到从某月份开始的天数和，我们调用`month-num`，它在`month`中查询天数，如果是在润年的二月之后，则加一。  
要找到从某年开始的天数和，`date->num`调用`year-num`，它返回某年一月一日相对于起始点（2000.01.01）所代表的天数。这个函数的工作方式是从传入的实参`y`年开始，朝着起始年（2000）往上或往下数。  

    (defun num->date (n)     
        (multiple-value-bind (y left) (num-year n)       
        (multiple-value-bind (m d) (num-month left y)         
        (values d m y))))    
    (defun num-year (n)     
        (if (< n 0)         
        (do* ((y (- yzero 1) (- y 1))               
        (d (- (year-days y)) (- d (year-days y))))              
        ((<= d n) (values y (- n d))))         
        (do* ((y yzero (+ y 1))               
        (prev 0 d)               
        (d (year-days y) (+ d (year-days y))))              
        ((> d n) (values y (- n prev))))))    
    (defun num-month (n y)     
        (if (leap? y)         
        (cond ((= n 59) (values 2 29))               
        ((> n 59) (nmon (- n 1)))               
        (t        
            (nmon n)))         
            (nmon n)))    
    (defun nmon (n)     
        (let ((m (position n month :test #'<)))       
        (values m (+ 1 (- n (svref month (- m 1)))))))    
    (defun date+ (d m y n)     
        (num->date (+ (date->num d m y) n))) 
    
**图 5.2 日期运算：转换数字至日期**  

图 5.2 展示了代码的下半部份。函数`num->date`将整数转换回日期。它调用了`num-year`函数，以日期的格式返回年，以及剩余的天数。再将剩余的天数传给`num-month`，分解出月与日。  
和`year-num`相同，`num-year`从起始年往上或下数，一次数一年。并持续累积天数，直到它获得一个绝对值大于或等于`n`的数。如果它往下数，那么它可以返回当前迭代中的数值。不然它会超过年份，然后必须返回前次迭代的数值。这也是为什么要使用`prev`，`prev`在每次迭代时会存入`days`前次迭代的数值。  
函数`num-month`以及它的子程序（subroutine）`nmon`的行为像是相反地`month-num`。他们从常数向量`month`的数值到位置，然而`month-num`从位置到数值。  
图 5.2 的前两个函数可以合而为一。与其返回数值给另一个函数，`num-year`可以直接调用`num-month`。现在分成两部分的代码，比较容易做交互测试，但是现在它可以工作了，下一步或许是把它合而为一。  
有了`date->num`与`num->date`，日期运算是很简单的。我们在`date+`里使用它们，可以从特定的日期做加减。如果我们想透过`date+`来知道 1997 年 12 月 17 日六十天之后的日期:  

    > (multiple-value-list (date+ 17 12 1997 60))   
    (15 2 1998)  
    
我们得到，1998 年 2 月 15 日。  

Chapter 5 总结 (Summary) 
----------------------------

1. Common Lisp 有三个基本的区块建构子：`progn`；允许返回的`block`；以及允许`goto`的`tagbody`。很多内置的操作符隐含在区块里。  
2. 进入一个新的词法语境，概念上等同于函数调用。  
3. Common Lisp 提供了适合不同情况的条件式。每个都可以使用`if`来定义。  
4. 有数个相似迭代操作符的变种。  
5. 表达式可以返回多个数值。  
6. 计算过程可以被中断以及保护，保护可使其免于中断所造成的后果。  

Chapter 5 练习 (Exercises) 
----------------------------

1. 将下列表达式翻译成没有使用`let`与`let\*`，并使同样的表达式不被求值 2 次。

(a) 
<!--  -->

    (let ((x (car y)))         
        (cons x x))   

(b)
<!--  -->
    (let* ((w (car x))              
        (y (+ w z)))         
        (cons w y))  

2. 使用`cond`重写 29 页的`mystery`函数。（译注: 第二章的练习第 5 题的 (b) 部分)  
3. 定义一个返回其实参平方的函数，而当实参是一个正整数且小于等于 5 时，不要计算其平方。  
4. 使用`case`与`svref`重写`month-num`(图 5.1)。  
5. 定义一个迭代与递归版本的函数，接受一个对象 x 与向量 v ，并返回一个列表，包含了向量 v 当中，所有直接在`x`之前的对象：

<!-- -->
    > (precedes #\a "abracadabra")
    (#\c #\d #\r)

6.  定义一个迭代与递归版本的函数，接受一个对象与列表，并返回一个新的列表，在原本列表的对象之间加上传入的对象：

<!-- -->

    > (intersperse '- '(a b c d))
    (A - B - C - D)

7.  定义一个接受一系列数字的函数，并在若且唯若每一对（pair）数字的差为一时，返回真，使用

<!-- -->

    (a) 递归
    (b) do
    (c) mapc 与 return

8.  定义一个单递归函数，返回两个值，分别是向量的最大与最小值。
9.  图 3.12
    的程序在找到一个完整的路径时，仍持续遍历伫列。在搜索范围大时，这可能会产生问题。

<!-- -->

    (a) 使用 catch 与 throw 来变更程序，使其找到第一个完整路径时，直接返回它。
    (b) 重写一个做到同样事情的程序，但不使用 catch 与 throw。
