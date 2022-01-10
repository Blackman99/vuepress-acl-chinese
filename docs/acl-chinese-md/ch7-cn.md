第七章：输入与输出
==================

Common Lisp 有着威力强大的 I/O
工具。针对输入以及一些普遍读取字符的函数，我们有 `read`
，包含了一个完整的解析器
(parser)。针对输出以及一些普遍写出字符的函数，我们有 `format`
，它自己几乎就是一个语言。本章介绍了所有基本的概念。

Common Lisp 有两种流
(streams)，字符流与二进制流。本章描述了字符流的操作；二进制流的操作涵盖在
14.2 节。

7.1 流 (Streams)
----------------

流是用来表示字符来源或终点的 Lisp
对象。要从文件读取或写入，你将文件作为流打开。但流与文件是不一样的。当你在顶层读入或印出时，你也可以使用流。你甚至可以创建可以读取或写入字符串的流。

输入缺省是从 `*standard-input*` 流读取。输出缺省是在 `*standard-output*`
流。最初它们大概会在相同的地方：一个表示顶层的流。

我们已经看过 `read` 与 `format`
是如何在顶层读取与印出。前者接受一个应是流的选择性参数，缺省是
`*standard-input*` 。 `format` 的第一个参数也可以是一个流，但当它是 `t`
时，输出被送到 `*standard-output*`
。所以我们目前为止都只用到缺省的流而已。我们可以在任何流上面做同样的 I/O
操作。

路径名（pathname）是一种指定一个文件的可移植方式。路径名包含了六个部分：host、device、directory、name、type
及 version。你可以通过调用 `make-pathname`
搭配一个或多个对应的关键字参数来产生一个路径。在最简单的情况下，你可以只指明名字，让其他的部分留为缺省：

    > (setf path (make-pathname :name "myfile"))
      #P"myfile"

开启一个文件的基本函数是 `open` 。它接受一个路径名 [^1]
以及大量的选择性关键字参数，而若是开启成功时，返回一个指向文件的流。

你可以在创建流时，指定你想要怎么使用它。
无论你是要写入流、从流读取或者同时进行读写操作，都可以通过 `direction`
参数设置。三个对应的数值是 `:input` , `:output` , `:io`
。如果是用来输出的流， `if-exists`
参数说明了如果文件已经存在时该怎么做；通常它应该是 `:supersede` (译注:
取代)。所以要创建一个可以写至 `"myfile"` 文件的流，你可以：

    > (setf str (open path :direction :output
                           :if-exists :supersede))
    #<Stream C017E6>

流的打印表示法因实现而异。

现在我们可以把这个流作为第一个参数传给 `format`
，它会在流印出，而不是顶层：

    > (format str "Something~%")
    NIL

如果我们在此时检查这个文件，可能有输出，也可能没有。某些实现会将输出累积成一块
(chunks)再输出。直到我们将流关闭，它也许一直不会出现：

    > (close str)
    NIL

当你使用完时，永远记得关闭文件；在你还没关闭之前，内容是不保证会出现的。现在如果我们检查文件
"myfile" ，应该有一行：

    Something

如果我们只想从一个文件读取，我们可以开启一个具有 `:direction :input`
的流 ：

    > (setf str (open path :direction :input))
    #<Stream C01C86>

我们可以对一个文件使用任何输入函数。7.2
节会更详细的描述输入。这里作为一个示例，我们将使用 `read-line`
从文件来读取一行文字：

    > (read-line str)
    "Something"
    > (close str)
    NIL

当你读取完毕时，记得关闭文件。

大部分时间我们不使用 `open` 与 `close` 来操作文件的 I/O 。
`with-open-file`
宏通常更方便。它的第一个参数应该是一个列表，包含了变数名、伴随着你想传给
`open`
的参数。在这之后，它接受一个代码主体，它会被绑定至流的变数一起被求值，其中流是通过将剩余的参数传给
`open`
来创建的。之后这个流会被自动关闭。所以整个文件写入动作可以表示为：

    (with-open-file (str path :direction :output
                              :if-exists :supersede)
      (format str "Something~%"))

`with-open-file` 宏将 `close` 放在 `unwind-protect` 里 (参见 92
页，译注: 5.6 节)，即使一个错误打断了主体的求值，文件是保证会被关闭的。

7.2 输入 (Input)
----------------

两个最受欢迎的输入函数是 `read-line` 及 `read` 。前者读入换行符
(newline)之前的所有字符，并用字符串返回它们。它接受一个选择性流参数
(optional stream argument)；若流忽略时，缺省为 `*standard-input*` :

    > (progn
        (format t "Please enter your name: ")
        (read-line))
    Please enter your name: Rodrigo de Bivar
    "Rodrigo de Bivar"
    NIL

译注：Rodrigo de Bivar 人称熙德 (El Cid)，十一世纪的西班牙民族英雄。

如果你想要原封不动的输出，这是你该用的函数。(第二个返回值只在
`read-line` 在遇到换行符之前，用尽输入时返回真。)

在一般情况下， `read-line` 接受四个选择性参数:
一个流；一个参数用来决定遇到 `end-of-file`
时，是否产生错误；若前一个参数为 `nil` 时，该返回什么；第四个参数 (在
235 页讨论)通常可以省略。

所以要在顶层显示一个文件的内容，我们可以使用下面这个函数：

    (defun pseudo-cat (file)
      (with-open-file (str file :direction :input)
        (do ((line (read-line str nil 'eof)
                   (read-line str nil 'eof)))
            ((eql line 'eof))
          (format t "~A~%" line))))

如果我们想要把输入解析为 Lisp 对象，使用 `read`
。这个函数恰好读取一个表达式，在表达式结束时停止读取。所以可以读取多于或少于一行。而当然它所读取的内容必须是合法的
Lisp 语法。

如果我们在顶层使用 `read`
，它会让我们在表达式里面，想用几个换行符就用几个：

    > (read)
    (a
    b
    c)
    (A B C)

换句话说，如果我们在一行里面输入许多表达式， `read`
会在第一个表达式之后，停止处理字符，留下剩余的字符给之后读取这个流的函数处理。所以如果我们在一行输入多个表达式，来回应
`ask-number` (20 页。译注：2.10 小节)所印出提示符，会发生如下情形:

    > (ask-number)
    Please enter a number. a b
    Please enter a number. Please enter a number. 43
    43

两个连续的提示符 (successive prompts)在第二行被印出。第一个 `read`
调用会返回 `a` ，而它不是一个数字，所以函数再次要求一个数字。但第一个
`read` 只读取到 `a` 的结尾。所以下一个 `read` 调用返回 `b`
，导致了下一个提示符。

你或许想要避免使用 `read` 来直接处理使用者的输入。前述的函数若使用
`read-line` 来获得使用者输入会比较好，然后对结果字符串调用
`read-from-string` 。这个函数接受一个字符串，并返回第一个读取的表达式:

    > (read-from-string "a b c")
    A
    2

它同时返回第二个值，一个指出停止读取字符串时的位置的数字。

在一般情况下， `read-from-string`
可以接受两个选择性参数与三个关键字参数。两个选择性参数是 `read`
的第三、第四个参数: 一个 end-of-file (这个情况是字符串)
決定是否报错，若不报错该返回什么。关键字参数 `:start` 及 `:end`
可以用来划分从字符串的哪里开始读。

所有的这些输入函数是由基本函数 (primitive) `read-char`
所定义的，它读取一个字符。它接受四个与 `read` 及 `read-line`
一样的选择性参数。Common Lisp 也定义一个函数叫做 `peek-char` ，跟
`read-char` 类似，但不会将字符从流中移除。

7.3 输出 (Output)
-----------------

三个最简单的输出函数是 `prin1` , `princ` 以及 `terpri`
。这三个函数的最后一个参数皆为选择性的流参数，缺省是 `*standard-output*`
。

`prin1` 与 `princ` 的差别大致在于 `prin1` 给程序产生输出，而 `princ`
给人类产生输出。所以举例来说， `prin1` 会印出字符串左右的双引号，而
`princ` 不会:

    > (prin1 "Hello")
    "Hello"
    "Hello"
    > (princ "Hello")
    Hello
    "Hello"

两者皆返回它们的第一个参数 (译注: 第二个值是返回值) ── 顺道一提，是用
`prin1` 印出。 `terpri` 仅印出一新行。

有这些函数的背景知识在解释更为通用的 `format`
是很有用的。这个函数几乎可以用在所有的输出。他接受一个流 (或 `t` 或
`nil` )、一个格式化字符串 (format
string)以及零个或多个额外的参数。格式化字符串可以包含特定的格式化指令
(format directives)，这些指令前面有波浪号 `~`
。某些格式化指令作为字符串的占位符
(placeholder)使用。这些位置会被格式化字符串之后，所给入参数的表示法所取代。

如果我们把 `t` 作为第一个参数，输出会被送至 `*standard-output*`
。如果我们给 `nil` ， `format`
会返回一个它会如何印出的字符串。为了保持简短，我们会在所有的示例里演示怎么做。

由于每人的观点不同， `format`
可以是令人惊讶的强大或是极为可怕的复杂。有大量的格式化指令可用，而只有少部分会被大多数程序设计师使用。两个最常用的格式化指令是
`~A` 以及 `~%` 。(你使用 `~a` 或 `~A`
都没关系，但后者较常见，因为它让格式化指令看起来一目了然。) 一个 `~A`
是一个值的占位符，它会像是用 `princ` 印出一般。一个 `~%`
代表着一个换行符 (newline)。

    > (format nil "Dear ~A, ~% Our records indicate..."
                          "Mr. Malatesta")
      "Dear Mr. Malatesta,
         Our records indicate..."

这里 `format` 返回了一个值，由一个含有换行符的字符串组成。

`~S` 格式化指令像是 `~A` ，但它使用 `prin1` 印出对象，而不是 `princ`
印出:

    > (format t "~S  ~A" "z" "z")
    "z" z
    NIL

格式化指令可以接受参数。 `~F` 用来印出向右对齐
(right-justified)的浮点数，可接受五个参数:

1.  要印出字符的总数。缺省是数字的长度。
2.  小数之后要印几位数。缺省是全部。
3.  小数点要往右移几位 (即等同于将数字乘 10)。缺省是没有。
4.  若数字太长无法满足第一个参数时，所要印出的字符。如果没有指定字符，一个过长的数字会尽可能使用它所需的空间被印出。
5.  数字开始印之前左边的字符。缺省是空白。

下面是一个有五个参数的罕见例子:

    ? (format nil "~10,2,0,'*,' F" 26.21875)
    "     26.22"

这是原本的数字取至小数点第二位、(小数点向左移 0 位)、在 10
个字符的空间里向右对齐，左边补满空白。注意作为参数给入是写成 `'*` 而不是
`#\*` 。由于数字塞得下 10 个字符，不需要使用第四个参数。

所有的这些参数都是选择性的。要使用缺省值你可以直接忽略对应的参数。如果我们想要做的是，印出一个小数点取至第二位的数字，我们可以说:

    > (format nil "~,2,,,F" 26.21875)
    "26.22"

你也可以忽略一系列的尾随逗号 (trailing
commas)，前面指令更常见的写法会是:

    > (format nil "~,2F" 26.21875)
    "26.22"

**警告:** 当 `format` 取整数时，它不保证会向上进位或向下舍入。就是说
`(format nil "~,1F" 1.25)` 可能会是 `"1.2"` 或 `"1.3"` 。所以如果你使用
`format` 来显示资讯时，而使用者期望看到某种特定取整数方式的数字 (如:
金额数量)，你应该在印出之前先显式地取好整数。

7.4 示例：字符串代换 (Example: String Substitution)
---------------------------------------------------

作为一个 I/O
的示例，本节演示如何写一个简单的程序来对文本文件做字符串替换。我们即将写一个可以将一个文件中，旧的字符串
`old` 换成某个新的字符串 `new`
的函数。最简单的实现方式是将输入文件里的每一个字符与 `old`
的第一个字符比较。如果没有匹配，我们可以直接印出该字符至输出。如果匹配了，我们可以将输入的下一个字符与
`old` 的第二个字符比较，等等。如果输入字符与 `old`
完全相等时，我们有一个成功的匹配，则我们印出 `new` 至文件。

而要是 `old`
在匹配途中失败了，会发生什么事呢？举例来说，假设我们要找的模式
(pattern)是 `"abac"` ，而输入文件包含的是 `"ababac"`
。输入会一直到第四个字符才发现不匹配，也就是在模式中的 `c` 以及输入的
`b` 才发现。在此时我们可以将原本的 `a`
写至输出文件，因为我们已经知道这里没有匹配。但有些我们从输入读入的字符还是需要留着:
举例来说，第三个 `a`
，确实是成功匹配的开始。所以在我们要实现这个算法之前，我们需要一个地方来储存，我们已经从输入读入的字符，但之后仍然需要的字符。

一个暂时储存输入的队列 (queue)称作缓冲区
(buffer)。在这个情况里，因为我们知道我们不需要储存超过一个预定的字符量，我们可以使用一个叫做环状缓冲区
`ring buffer`
的资料结构。一个环状缓冲区实际上是一个向量。是使用的方式使其成为环状:
我们将之后的元素所输入进来的值储存起来，而当我们到达向量结尾时，我们重头开始。如果我们不需要储存超过
`n` 个值，则我们只需要一个长度为 `n` 或是大于 `n`
的向量，这样我们就不需要覆写正在用的值。

在图 7.1 的代码，实现了环状缓冲区的操作。 `buf` 有五个字段 (field):
一个包含存入缓冲区的向量，四个其它字段用来放指向向量的索引
(indices)。两个索引是 `start` 与 `end`
，任何环状缓冲区的使用都会需要这两个索引: `start`
指向缓冲区的第一个值，当我们取出一个值时， `start` 会递增
(incremented)； `end` 指向缓冲区的最后一个值，当我们插入一个新值时，
`end` 会递增。

另外两个索引， `used` 以及 `new`
，是我们需要给这个应用的基本环状缓冲区所加入的东西。它们会介于 `start`
与 `end` 之间。实际上，它总是符合

    start ≤ used ≤ new ≤ end

你可以把 `used` 与 `new` 想成是当前匹配 (current match) 的 `start` 与
`end` 。当我们开始一轮匹配时， `used` 会等于 `start` 而 `new` 会等于
`end` 。当下一个字符 (successive character)匹配时，我们需要递增 `used`
。当 `used` 与 `new`
相等时，我们将开始匹配时，所有存在缓冲区的字符读入。我们不想要使用超过从匹配时所存在缓冲区的字符，或是重复使用同样的字符。因此这个
`new` 索引，开始等于 `end`
，但它不会在一轮匹配我们插入新字符至缓冲区一起递增。

函数 `bref` 接受一个缓冲区与一个索引，并返回索引所在位置的元素。借由使用
`index` 对向量的长度取 `mod`
，我们可以假装我们有一个任意长的缓冲区。调用 `(new-buf n)`
会产生一个新的缓冲区，能够容纳 `n` 个对象。

要插入一个新值至缓冲区，我们将使用 `buf-insert` 。它将 `end`
递增，并把新的值放在那个位置 (译注: 递增完的位置)。相反的 `buf-pop`
返回一个缓冲区的第一个数值，接着将 `start`
递增。任何环状缓冲区都会有这两个函数。

    (defstruct buf
      vec (start -1) (used -1) (new -1) (end -1))

    (defun bref (buf n)
      (svref (buf-vec buf)
             (mod n (length (buf-vec buf)))))

    (defun (setf bref) (val buf n)
      (setf (svref (buf-vec buf)
                   (mod n (length (buf-vec buf))))
            val))

    (defun new-buf (len)
      (make-buf :vec (make-array len)))

    (defun buf-insert (x b)
      (setf (bref b (incf (buf-end b))) x))

    (defun buf-pop (b)
      (prog1
        (bref b (incf (buf-start b)))
        (setf (buf-used b) (buf-start b)
              (buf-new  b) (buf-end   b))))

    (defun buf-next (b)
      (when (< (buf-used b) (buf-new b))
        (bref b (incf (buf-used b)))))

    (defun buf-reset (b)
      (setf (buf-used b) (buf-start b)
            (buf-new  b) (buf-end   b)))

    (defun buf-clear (b)
      (setf (buf-start b) -1 (buf-used  b) -1
            (buf-new   b) -1 (buf-end   b) -1))

    (defun buf-flush (b str)
      (do ((i (1+ (buf-used b)) (1+ i)))
          ((> i (buf-end b)))
        (princ (bref b i) str)))

**图 7.1 环状缓冲区的操作**

接下来我们需要两个特别为这个应用所写的函数: `buf-next`
从缓冲区读取一个值而不取出，而 `buf-reset` 重置 `used` 与 `new`
到初始值，分别是 `start` 与 `end` 。如果我们已经把至 `new`
的值全部读取完毕时， `buf-next` 返回 `nil`
。区别这个值与实际的值不会产生问题，因为我们只把值存在缓冲区。

最后 `buf-flush` 透过将所有作用的元素，写至由第二个参数所给入的流，而
`buf-clear` 通过重置所有的索引至 `-1` 将缓冲区清空。

在图 7.1 定义的函数被图 7.2 所使用，包含了字符串替换的代码。函数
`file-subst`
接受四个参数；一个查询字符串，一个替换字符串，一个输入文件以及一个输出文件。它创建了代表每个文件的流，然后调用
`stream-subst` 来完成实际的工作。

第二个函数 `stream-subst`
使用本节开始所勾勒的算法。它一次从输入流读一个字符。直到输入字符匹配要寻找的字符串时，直接写至输出流
(1)。当一个匹配开始时，有关字符在缓冲区 `buf` 排队等候 (2)。

变数 `pos` 指向我们想要匹配的字符在寻找字符串的所在位置。如果 `pos`
等于这个字符串的长度，我们有一个完整的匹配，则我们将替换字符串写至输出流，并清空缓冲区
(3)。如果在这之前匹配失败，我们可以将缓冲区的第一个元素取出，并写至输出流，之后我们重置缓冲区，并从
`pos` 等于 0 重新开始 (4)。

    (defun file-subst (old new file1 file2)
      (with-open-file (in file1 :direction :input)
         (with-open-file (out file2 :direction :output
                                    :if-exists :supersede)
           (stream-subst old new in out))))

    (defun stream-subst (old new in out)
      (let* ((pos 0)
             (len (length old))
             (buf (new-buf len))
             (from-buf nil))
        (do ((c (read-char in nil :eof)
                (or (setf from-buf (buf-next buf))
                    (read-char in nil :eof))))
            ((eql c :eof))
          (cond ((char= c (char old pos))
                 (incf pos)
                 (cond ((= pos len)            ; 3
                        (princ new out)
                        (setf pos 0)
                        (buf-clear buf))
                       ((not from-buf)         ; 2
                        (buf-insert c buf))))
                ((zerop pos)                   ; 1
                 (princ c out)
                 (when from-buf
                   (buf-pop buf)
                   (buf-reset buf)))
                (t                             ; 4
                 (unless from-buf
                   (buf-insert c buf))
                 (princ (buf-pop buf) out)
                 (buf-reset buf)
                 (setf pos 0))))
        (buf-flush buf out)))

**图 7.2 字符串替换**

下列表格展示了当我们将文件中的 `"baro"` 替换成 `"baric"`
所发生的事，其中文件只有一个单字 `"barbarous"` :


| CHARACTER    | SOURCE      | MATCH    | CASE    | OUTPUT    | BUFFER        |
| --- | -- | ---| --- | --- | --- |
| b            | file        | > b      | > 2     |           | b             |
| a            | file        | > a      | > 2     |           | b a           |
| r            | file        | > r      | > 2     |           | b a r         |
| b            | file        | > o      | > 4     | b         | b.a r b.      |
| a            | buffer      | > b      | > 1     | a         | a.r b.        |
| r            | buffer      | > b      | > 1     | r         | r.b.          |
| b            | buffer      | > b      | > 1     |           | r b:          |
| a            | file        | > a      | > 2     |           | r b:a         |
| r            | file        | > r      | > 2     |           | r b:a         |
| o            | file        | > o      | > 3     | baric     | r b:a r       |
| u            | file        | > b      | > 1     | u         |               |
| a            | file        | > b      | > 1     | s         |               |

第一栏是当前字符 ── `c`
的值；第二栏显示是从缓冲区或是直接从输入流读取；第三栏显示需要匹配的字符
── `old` 的第 **posth** 字符；第四栏显示那一个条件式
(case)被求值作为结果；第五栏显示被写至输出流的字符；而最后一栏显示缓冲区之后的内容。在最后一栏里，
`used` 与 `new` 的位置一样，由一个冒号 ( `:` colon)表示。

在文件 `"test1"` 里有如下文字：

    The struggle between Liberty and Authority is the most conspicuous feature
    in the portions of history with which we are earliest familiar, particularly
    in that of Greece, Rome, and England.

在我们对 `(file-subst " th" " z" "test1" "test2")` 求值之后，读取文件
`"test2"` 为:

    The struggle between Liberty and Authority is ze most conspicuous feature
    in ze portions of history with which we are earliest familiar, particularly
    in zat of Greece, Rome, and England.

为了使这个例子尽可能的简单，图 7.2
的代码只将一个字符串换成另一个字符串。很容易扩展为搜索一个模式而不是一个字面字符串。你只需要做的是，将
`char=` 调用换成一个你想要的更通用的匹配函数调用。

7.5 宏字符 (Macro Characters)
-----------------------------

一个宏字符 (macro character)是获得 `read` 特别待遇的字符。比如小写的 `a`
，通常与小写 `b` 一样处理，但一个左括号就不同了: 它告诉 Lisp
开始读入一个列表。

一个宏字符或宏字符组合也称作 `read-macro` (读取宏) 。许多 Common Lisp
预定义的读取宏是缩写。比如说引用 (Quote): 读入一个像是 `'a`
的表达式时，它被读取器展开成 `(quote a)` 。当你输入引用的表达式 (quoted
expression)至顶层时，它们在读入之时就会被求值，所以一般来说你看不到这样的转换。你可以透过显式调用
`read` 使其现形:

    > (car (read-from-string "'a"))
    QUOTE

引用对于读取宏来说是不寻常的，因为它用单一字符表示。有了一个有限的字符集，你可以在
Common Lisp 里有许多单一字符的读取宏，来表示一个或更多字符。

这样的读取宏叫做派发 (dispatching)读取宏，而第一个字符叫做派发字符
(dispatching character)。所有预定义的派发读取宏使用井号 ( `#`
)作为派发字符。我们已经见过好几个。举例来说， `#'` 是 `(function ...)`
的缩写，同样的 `'` 是 `(quote ...)` 的缩写。

其它我们见过的派发读取宏包括 `#(...)` ，产生一个向量； `#nA(...)`
产生数组； `#\` 产生一个字符； `#S(n ...)`
产生一个结构。当这些类型的每个对象被 `prin1` 显示时 (或是 `format` 搭配
`~S`)，它们使用对应的读取宏 [^2] 。这表示着你可以写出或读回这样的对象:

    > (let ((*print-array* t))
        (vectorp (read-from-string (format nil "~S"
                                           (vector 1 2)))))
    T

当然我们拿回来的不是同一个向量，而是具有同样元素的新向量。

不是所有对象被显示时都有着清楚
(distinct)、可读的形式。举例来说，函数与哈希表，倾向于这样 `#<...>`
被显示。实际上 `#<...>` 也是一个读取宏，但是特别用来产生当遇到 `read`
的错误。函数与哈希表不能被写出与读回来，而这个读取宏确保使用者不会有这样的幻觉。
[^3]

当你定义你自己的事物表示法时
(举例来说，结构的印出函数)，你要将此准则记住。要不使用一个可以被读回来的表示法，或是使用
`#<...>` 。

Chapter 7 总结 (Summary)
------------------------

1.  流是输入的来源或终点。在字符流里，输入输出是由字符组成。
2.  缺省的流指向顶层。新的流可以由开启文件产生。
3.  你可以解析对象、字符组成的字符串、或是单独的字符。
4.  `format` 函数提供了完整的输出控制。
5.  为了要替换文本文件中的字符串，你需要将字符读入缓冲区。
6.  当 `read` 遇到一个宏字符像是 `'` ，它调用相关的函数。

Chapter 7 练习 (Exercises)
--------------------------

1.  定义一个函数，接受一个文件名并返回一个由字符串组成的列表，来表示文件里的每一行。
2.  定义一个函数，接受一个文件名并返回一个由表达式组成的列表，来表示文件里的每一行。
3.  假设有某种格式的文件文件，注解是由 `%`
    字符表示。从这个字符开始直到行尾都会被忽略。定义一个函数，接受两个文件名称，并拷贝第一个文件的内容去掉注解，写至第二个文件。
4.  定义一个函数，接受一个二维浮点数组，将其用简洁的栏位显示。每个元素应印至小数点二位，一栏十个字符宽。（假设所有的字符可以容纳）。你会需要
    `array-dimensions` (参见 361 页，译注: Appendix D)。
5.  修改 `stream-subst` 来允许万用字符 (wildcard)
    可以在模式中使用。若字符 `+` 出现在 `old`
    里，它应该匹配任何输入字符。
6.  修改 `stream-subst`
    来允许模式可以包含一个用来匹配任何数字的元素，以及一个可以匹配任何英文字符的元素或是一个可以匹配任何字符的元素。模式必须可以匹配任何特定的输入字符。(提示:
    `old` 可以不是一个字符串。)

**脚注**

[^1]: 你可以给一个字符串取代路径名，但这样就不可携了 (portable)。

[^2]: 要让向量与数组这样被显示，将 `*print-array*` 设为真。

[^3]: Lisp 不能只用 `#'` 来表示函数，因为 `#'`
    本身无法提供表示闭包的方式。
