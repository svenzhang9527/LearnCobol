# COBOL 数据类型示例

以下是 COBOL 中各种数据类型的例子，涵盖了常见的数值类型、字符类型、日期和时间类型、以及布尔类型的使用方式。

## 1. 数值类型（Numeric Types）

- **PIC 9**：用于定义数字字段。
  
  ```cobol
  01 NUM1       PIC 9(5) VALUE 12345.
  ```

  定义了一个最多 5 位的数字变量 `NUM1`，并初始化为 `12345`。

- **PIC 9(5)V99**：用于定义带小数的数字。
  
  ```cobol
  01 AMOUNT     PIC 9(5)V99 VALUE 12345.67.
  ```

  `AMOUNT` 是一个 5 位整数和 2 位小数的数字字段，初始化为 `12345.67`。

- **S9**：定义有符号的数字。
  
  ```cobol
  01 BALANCE    PIC S9(5) VALUE -10000.
  ```

  `BALANCE` 是一个最多 5 位的带符号数字，初始化为 `-10000`。

## 2. 字符类型（Alphanumeric Types）

- **PIC X**：定义单个字符。
  
  ```cobol
  01 GRADE      PIC X VALUE 'A'.
  ```

  定义了一个字符型变量 `GRADE`，初始化为字符 `'A'`。

- **PIC X(10)**：定义一个长度为 10 的字符型字段。
  
  ```cobol
  01 NAME       PIC X(10) VALUE 'John Doe'.
  ```

  定义了一个最多 10 个字符的变量 `NAME`，初始化为 `'John Doe'`。

- **PIC A**：用于字母，通常与 PIC X 一起使用。
  
  ```cobol
  01 GENDER     PIC A VALUE 'M'.
  ```

  定义了一个字符型变量 `GENDER`，初始化为字母 `'M'`。

## 3. 日期和时间类型

- **PIC 9(8)**：表示日期，通常为 YYYYMMDD 格式。
  
  ```cobol
  01 BIRTHDATE  PIC 9(8) VALUE 19901225.
  ```

  定义了一个 8 位的日期字段 `BIRTHDATE`，格式为 `19901225`（即 1990 年 12 月 25 日）。

- **PIC 9(6)**：表示时间，通常为 HHMMSS 格式。
  
  ```cobol
  01 STARTTIME  PIC 9(6) VALUE 093000.
  ```

  定义了一个 6 位的时间字段 `STARTTIME`，格式为 `093000`（即 09:30:00）。

## 4. 布尔类型（Boolean Type）

COBOL 没有原生的布尔类型，但可以用字符类型表示布尔值。

- 使用 **PIC X** 类型来表示布尔值。
  
  ```cobol
  01 ISVALID    PIC X(1) VALUE 'Y'.  *> Y 表示真，N 表示假
  ```

  `ISVALID` 变量用字符 `'Y'` 表示“真”，用 `'N'` 表示“假”。

## 5. 组合数据类型

COBOL 允许通过组合基本数据类型来构建更复杂的数据结构。

```cobol
01 EMPLOYEE.
   05 EMP-ID    PIC 9(6).
   05 EMP-NAME  PIC X(20).
   05 EMP-AGE   PIC 9(2).
```

在这个例子中，`EMPLOYEE` 是一个复合结构，包含员工的 ID、姓名和年龄。
