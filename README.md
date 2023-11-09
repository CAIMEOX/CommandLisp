# Command Lisp Document
> The Project is WIP actively

​	Command Lisp是一门为Minecraft Bedrock设计的简化语言，拥有这非常高的抽象程度。同时，它也是Lisp的一门方言。

> Lisp 与众不同的部分原因是，它被设计成能够自己进化。你能用 Lisp 定义新的 Lisp 操作符。当新的抽象概念风行时（如面向对象程序设计），我们总是发现这些新概念在 Lisp 是最容易来实现的。Lisp 就像生物的 DNA 一样，这样的语言永远不会过时。

### 为什么要使用Command Lisp (CL)?

​	在Minecraft Bedrock中编辑命令有时候非常困难，不稳定的游戏代码也给我们造成了很多麻烦，而且很多命令非常不直观，大大降低了工作效率。CL的诞生就是为了解决这些痛点，使用它你能体会到如下诸多好处。

- 一次编写，多处运行：你只需要使用CL完成命令编写，可以利用我们的工具导入多个存档，进而实现复用。
- 高抽象，高效率：CL语言让你能够以更少的代码量实现更多的可能性
- 简单易学，Lisp语言非常直观易懂
- 减少错误：相比直接在命令系统寻找错误，在CL脚本中寻找错误显然更加简单

## 变量定义

​	CommandLisp有以下几种变量类型：

- number 数字类型

- string 字符串

- atom 原子

  可以使用let来定义临时变量:

  ```lisp
  (let (a 2)) ; a = 2
  (let (b "tell @e Secret Message!")(cmd b)) ; "tell @e Secret Message!"
  ```

  define则用于普通变量：

  ```lisp
  (define a 2)
  (if (> a 2)(write "a > 2")(write "a <= 2"))
  ```

  

## 基本命令

- tag+ : 给实体添加tag
- tag- : 给实体移除tag
- tag~: 判断实体存在某tag

```lisp
(tag+ p "TAG_1") ;tag @p add TAG_1
(tag- r "TAG_2") ;tag @p remove TAG_2
(tag~ r "TAG_3") ;testfor @r[tag=TAG_3] ~ ~ ~ 
```

- quote (')

  一个不遵守 Command Lisp 求值规则的操作符是 `quote` 。 `quote` 是一个特殊的操作符，意味着它自己有一套特别的求值规则。这个规则就是：什么也不做。 `quote` 操作符接受一个实参，并完封不动地返回它。

  ```lisp
  ;返回(tag+ p "TAG_3")
  (quote (tag+ p "TAG_3")) 
  ('(tag+ p "TAG_3")) 
  ```

- cmd

  执行某个命令，不推荐使用，因为Command Lisp对大量命令都有封装

  ```lisp
  (cmd "say Hello World") ;"say Hello World"
  (cmd "kill @e") ; "kill @e"
  ```

- write

  标准聊天窗口输出

  ```lisp
  (write "Hello World") ; [!]Hello World
  ```

- tp

  Teleport命令

  ```lisp
  (tp r r) ; "tp @r @r"
  (tp a p) ; "tp @a @p"
  ```

## 条件表达式

​	Command Lisp内置了一些方便的条件式，最简单的当然是if，它通常接受三个实参，一个 *test* 表达式，一个 *then* 表达式和一个 *else* 表达式。若 `test` 表达式求值为逻辑 `真` ，则对 `then` 表达式求值，并返回这个值（通常是信号）；若 `test` 表达式求值为逻辑 `假` ，则对 `else` 表达式求值。

```lisp
(if (test)(then)(else))
(if (tag~ a "TEST")(write "Found player marked for TEST")(write "No player has the TEST tag"))
```

## License
GPL V3

