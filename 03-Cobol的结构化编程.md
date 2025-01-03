# COBOL 结构化编程

COBOL 的结构化编程是一种通过明确的控制结构和模块化设计来提高程序可读性和可维护性的方法。尽管 COBOL 最初是为批处理和数据处理任务设计的，但随着编程实践的发展，结构化编程在 COBOL 中得到了广泛应用。以下是 COBOL 中常见的结构化编程元素：

## 1. 程序的四个主要部分

COBOL 程序被分为四个主要部分，每个部分有特定的作用，支持结构化编程：

- **IDENTIFICATION DIVISION**: 程序的元数据部分，用于声明程序的名称、作者、版本等信息。
- **ENVIRONMENT DIVISION**: 描述程序运行的环境，通常包括输入输出文件和硬件设备的定义。
- **DATA DIVISION**: 定义数据结构和变量。
- **PROCEDURE DIVISION**: 编写实际的逻辑处理代码，包括数据处理、运算和输出。

## 2. 控制结构

结构化编程要求明确使用控制结构来控制程序流。COBOL 支持多种控制结构，如条件语句、循环语句和过程调用。

### 2.1 条件语句（IF-ELSE）

`IF` 语句用于进行条件判断，是结构化编程的基本元素。COBOL 中的 `IF` 语句遵循明确的结构：

```cobol
IF condition
    statement-1
ELSE
    statement-2
END-IF.
```

#### 示例：

```cobol
IF NUM1 > NUM2
    DISPLAY 'NUM1 is greater'.
ELSE
    DISPLAY 'NUM2 is greater'.
END-IF.
```

### 2.2 循环语句（PERFORM）

`PERFORM` 用于执行一段代码多次，可以通过条件控制执行次数，支持以下形式：

- **简单循环**（PERFORM UNTIL 或 PERFORM VARYING）

  ```cobol
  PERFORM UNTIL condition
      statement.
  END-PERFORM.
  ```

  ```cobol
  PERFORM VARYING counter FROM 1 BY 1 UNTIL counter > 10
      DISPLAY counter.
  END-PERFORM.
  ```

- **过程调用**：通过 `PERFORM` 调用一个过程（子程序）。

  ```cobol
  PERFORM ADDITION.
  ```

### 2.3 选择语句（EVALUATE）

`EVALUATE` 是 COBOL 中的多重选择结构，相当于其他语言中的 `switch` 或 `case`。

```cobol
EVALUATE TRUE
    WHEN condition-1
        statement-1
    WHEN condition-2
        statement-2
    WHEN OTHER
        statement-3
END-EVALUATE.
```

#### 示例：

```cobol
EVALUATE GRADE
    WHEN 'A'
        DISPLAY 'Excellent'.
    WHEN 'B'
        DISPLAY 'Good'.
    WHEN 'C'
        DISPLAY 'Satisfactory'.
    WHEN OTHER
        DISPLAY 'Poor'.
END-EVALUATE.
```

## 3. 模块化编程

结构化编程鼓励将代码分成模块，每个模块负责特定的任务。COBOL 支持通过过程（`PERFORM`）和子程序来实现模块化。

### 3.1 使用 PERFORM 调用子程序

在 COBOL 中，程序的不同部分可以被定义为子程序（PROCEDURE DIVISION 中的过程），并通过 `PERFORM` 语句调用。

```cobol
PROCEDURE DIVISION.
   PERFORM ADDITION.

ADD-PROCESS.
   ADD NUM1 TO NUM2 GIVING RESULT.
   DISPLAY RESULT.
   RETURN.
```

### 3.2 使用段（Paragraphs）

COBOL 允许将程序分为多个段（paragraph），每个段执行特定的功能，可以通过 `PERFORM` 语句来调用。

```cobol
PROCEDURE DIVISION.
   PERFORM PROCESS-ONE.
   PERFORM PROCESS-TWO.

PROCESS-ONE.
   DISPLAY 'This is process one.'.

PROCESS-TWO.
   DISPLAY 'This is process two.'.
```

### 3.3 使用 CALL 调用外部程序

COBOL 还支持调用外部程序或子程序。这允许程序更加模块化和可复用。

```cobol
CALL 'EXTERNAL-PROGRAM' USING var1 var2.
```

## 4. 错误处理和异常处理

结构化编程强调使用清晰的错误处理机制。COBOL 中的 `ON ERROR` 语句可以用来捕获和处理错误。

```cobol
ACCEPT user-input.
IF user-input = ERROR
    DISPLAY 'Error: Invalid input.'
END-IF.
```

## 5. 数据定义和结构

结构化编程中，数据的定义必须清晰且一致。COBOL 强调在 DATA DIVISION 中定义所有数据项，并通过数据描述符（如 PIC）来确保数据格式的正确性。

```cobol
DATA DIVISION.
WORKING-STORAGE SECTION.
01 NUM1       PIC 9(5) VALUE 12345.
01 NUM2       PIC 9(5) VALUE 54321.
```

## 6. 代码注释和文档

结构化编程要求程序员在代码中加入适当的注释，以提高程序的可读性和可维护性。COBOL 使用 `*` 或 `*>` 进行注释。

```cobol
*> This is a comment
* This is another comment
```

## 7. 减少“GOTO”语句的使用

结构化编程强调避免过度使用 `GOTO` 语句，因为它可能导致程序流程不清晰，难以维护。在结构化 COBOL 编程中，应该尽量使用 `IF`、`EVALUATE`、`PERFORM` 等结构来替代 `GOTO`，提高程序的可读性和可维护性。
