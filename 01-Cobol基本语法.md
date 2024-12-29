# COBOL 基本语法

COBOL 的基本语法具有较强的结构化特点，要求程序员遵循一定的格式来编写代码。以下是 COBOL 的基本语法构成：

## 1. 程序结构

COBOL 程序通常由以下几个部分组成：
- **IDENTIFICATION DIVISION**: 程序的元数据，主要包括程序名称等信息。
- **ENVIRONMENT DIVISION**: 描述程序运行的外部环境，通常用于定义输入输出设备和文件。
- **DATA DIVISION**: 定义程序中的数据结构、变量和常量。
- **PROCEDURE DIVISION**: 包含实际的处理逻辑，编写程序的业务逻辑部分。

## 2. 基本语法规则

### 1. 代码行格式
- COBOL 程序由 80 列的行组成。
- 行的结构被划分为 4 个区域：区号（Area A）、标签区（Area B）、对象区（Area C）和保留区（Area D）。

  - **区号（Area A）**：占用前 6 个字符，通常用于标记段名、段标签等。
  - **标签区（Area B）**：从第 7 至第 72 列，通常放置 COBOL 语句。
  - **对象区（Area C）**：从第 73 至第 80 列，通常用作注释或保留。
  - **保留区（Area D）**：不常用，供编译器使用。

### 2. 区分大小写
- COBOL 是不区分大小写的语言，即 `DATA` 和 `data` 被视为相同。

### 3. 语句结束
- COBOL 语句通常以句号（.）结束。

### 4. 关键字
- COBOL 使用关键字，如 `IF`、`DISPLAY`、`ADD` 等。

### 5. 标识符
- 标识符由字母、数字和连字符组成，并且必须以字母开头。

## 3. 程序结构示例

一个简单的 COBOL 程序结构示例如下：

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ExampleProgram.
       
       ENVIRONMENT DIVISION.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 NUM1       PIC 9(5) VALUE 12345.
       01 NUM2       PIC 9(5) VALUE 54321.
       01 RESULT     PIC 9(5).
       
       PROCEDURE DIVISION.
       ADD NUM1 TO NUM2 GIVING RESULT.
       DISPLAY 'The result is: ' RESULT.
       STOP RUN.
```

## 4. 各个部分的说明

- **IDENTIFICATION DIVISION**: 定义程序名称 `ExampleProgram`。

  ```cobol
  IDENTIFICATION DIVISION.
  PROGRAM-ID. ExampleProgram.
  ```

- **ENVIRONMENT DIVISION**: 通常用来描述输入输出环境，但此例中为空。

  ```cobol
  ENVIRONMENT DIVISION.
  ```

- **DATA DIVISION**: 定义数据部分，包含变量的声明。例如：

  - `NUM1` 和 `NUM2` 都是 5 位的数字。
  - `RESULT` 用来存储 `NUM1` 和 `NUM2` 的加法结果。

  ```cobol
  DATA DIVISION.
  WORKING-STORAGE SECTION.
  01 NUM1       PIC 9(5) VALUE 12345.
  01 NUM2       PIC 9(5) VALUE 54321.
  01 RESULT     PIC 9(5).
  ```

- **PROCEDURE DIVISION**: 编写程序的实际操作逻辑。

  - 这里执行了两个数字相加，并将结果显示。

  ```cobol
  PROCEDURE DIVISION.
  ADD NUM1 TO NUM2 GIVING RESULT.
  DISPLAY 'The result is: ' RESULT.
  STOP RUN.
  ```

## 5. 常见语句

- **DISPLAY**: 显示输出到控制台。

  ```cobol
  DISPLAY 'Hello, COBOL World!'.
  ```

- **ADD**: 执行加法操作。

  ```cobol
  ADD NUM1 TO NUM2 GIVING RESULT.
  ```

- **IF**: 条件判断。

  ```cobol
  IF NUM1 > NUM2
      DISPLAY 'NUM1 is greater'.
  END-IF.
  ```

- **PERFORM**: 执行一个过程或子程序。

  ```cobol
  PERFORM ADDITION.
  ```

- **STOP RUN**: 结束程序执行。

  ```cobol
  STOP RUN.
  ```

## 6. 数据类型

COBOL 的数据类型分为：
- **数值类型**：如 `PIC 9`。
- **字符类型**：如 `PIC X`。
- **布尔类型**：通过字符字段表示，例如 'Y' 或 'N'。

## 总结

COBOL 的语法非常注重结构化，每一部分都有明确的角色。它要求严格的代码排版，且每一条语句都必须以句号结束。通过明确的关键字和数据类型定义，COBOL 适合用于大型、复杂的商业和金融应用。
