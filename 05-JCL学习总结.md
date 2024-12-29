# JCL（Job Control Language）

学习 JCL 是批量处理的基础，它是 IBM 主机上用于管理作业的语言。掌握 JCL 的语法、作业的提交、资源管理、条件检查等是非常关键的。

## 基本概念

- **JCL** 是 IBM 主机上的一种控制语言，用于描述批处理作业的执行方式。
- **作业(Job)**：由步骤（Step）组成，每个步骤执行一个任务。
- **步骤(Step)**：执行具体任务的基本单位，每个步骤都有可能引用指定的程序或系统命令。
- **作业控制语句**：JCL 包含多种控制语句，比如 `job`，`exec`，`dd` 等。用于定义作业的各个方面。

## 基本语法

- **Job 语句**：定义作业的开始，指定作业的属性（如：作业名，优先级等）
- **Exec 语句**：指定作业中的步骤，通常包含程序或命令的执行。
- **DD 语句**：定义数据集（如：输入文件，输出文件），包括文件名，数据集类型，存储位置等信息。
- **条件语句**：用于在作业中根据前一个步骤的执行结果进行条件判断，控制后续步骤的执行。

## 示例

通过示例来学习 JCL 是一个非常好的方法，下面我会给出一个简单的 JCL 示例，帮助你理解各个部分的作用：

### 示例 1：简单的 JCL 作业

这个例子展示了如何提交一个简单的作业，执行一个程序并处理输入输出数据集。

```jcl
//sven_JOB      JOB (ACCT#)：操作系统层面的账号,'NAME：描述，可改',CLASS=A（执行优先级）,MSGCLASS=A（日志优先级）
//sven_STEP1    EXEC PGM=MYPROGRAM（program的名称，可能是cobol程序的名称）
//INPUT    DD   DSN=MY.INPUT.FILE,DISP=SHR（共享，这个文件是只读的，其他任务也可以读）
//OUTPUT   DD   DSN=MY.OUTPUT.FILE,DISP=NEW
```

**解释**：

- `JOB` 语句：定义作业 `MYJOB`，`ACCT#` 是账户编号，`'NAME'` 是作业名称，`CLASS=A` 指定作业优先级，`MSGCLASS=A` 定义消息输出类别。
- `EXEC` 语句：定义执行步骤 `STEP1`，并指定要运行的程序 `MYPROGRAM`。
- `DD` 语句：定义数据集。`INPUT` 和 `OUTPUT` 是数据集的名称，`DISP=SHR` 表示输入文件可以共享，`DISP=NEW` 表示输出文件是新创建的。

### 示例 2：使用条件语句的 JCL

这个示例展示了如何使用条件语句，只有前一个步骤成功时才会执行后续步骤。

```jcl
//MYJOB    JOB (ACCT#),'NAME',CLASS=A,MSGCLASS=A
//STEP1    EXEC PGM=MYPROGRAM1
EXIT PROGRAM <返回码> 语句，有返回值。
//COND     COND=(0,NE)
//STEP2    EXEC PGM=MYPROGRAM2    
//INPUT    DD   DSN=MY.INPUT.FILE,DISP=SHR
//OUTPUT   DD   DSN=MY.OUTPUT.FILE,DISP=NEW
```

**解释**：

- `COND` 语句：`COND=(0,NE)` 表示只有前一个步骤（`STEP1`）执行成功（返回代码 0）时，才会执行后续步骤（`STEP2`）。
- 其他部分与前一个示例相同。

### 示例 3：带有多步骤的 JCL 作业

这个示例展示了如何通过多个步骤执行不同的程序，并指定多个数据集。

```jcl
//MYJOB    JOB (ACCT#),'NAME',CLASS=A,MSGCLASS=A
//STEP1    EXEC PGM=PROGRAM1
//IN1      DD   DSN=MY.INPUT1.FILE,DISP=SHR
//OUT1     DD   DSN=MY.OUTPUT1.FILE,DISP=NEW
//STEP2    EXEC PGM=PROGRAM2
//IN2      DD   DSN=MY.INPUT2.FILE,DISP=SHR
//OUT2     DD   DSN=MY.OUTPUT2.FILE,DISP=NEW
//STEP3    EXEC PGM=PROGRAM3
//IN3      DD   DSN=MY.INPUT3.FILE,DISP=SHR
//OUT3     DD   DSN=MY.OUTPUT3.FILE,DISP=NEW
```

**解释**：

- 该作业包含三个步骤（`STEP1`、`STEP2` 和 `STEP3`），每个步骤运行不同的程序。
- 每个步骤有不同的输入和输出文件。
- 你可以根据实际需求调整每个步骤的数据集和程序。

## 惯例

- **作业定义 → 步骤定义 → 数据集定义**


### `COND` 语句操作符

在 JCL 中，`COND` 语句用于根据前一个步骤的返回码（Return Code，RC）来控制后续步骤的执行。以下是 `COND` 语句中常用的操作符：

#### 常见操作符

1. **`EQ` (Equal)**  
   表示前一个步骤的返回码等于指定的值。

   ```jcl
   //COND COND=(0,EQ)
   ```
   - 只有当前一个步骤的返回码等于 0 时，才会执行当前步骤。

2. **`NE` (Not Equal)**  
   表示前一个步骤的返回码不等于指定的值。

   ```jcl
   //COND COND=(0,NE)
   ```
   - 只有当前一个步骤的返回码不等于 0 时，才会执行当前步骤。

3. **`LT` (Less Than)**  
   表示前一个步骤的返回码小于指定的值。

   ```jcl
   //COND COND=(8,LT)
   ```
   - 只有当前一个步骤的返回码小于 8 时，才会执行当前步骤。

4. **`LE` (Less Than or Equal)**  
   表示前一个步骤的返回码小于或等于指定的值。

   ```jcl
   //COND COND=(4,LE)
   ```
   - 只有当前一个步骤的返回码小于或等于 4 时，才会执行当前步骤。

5. **`GT` (Greater Than)**  
   表示前一个步骤的返回码大于指定的值。

   ```jcl
   //COND COND=(2,GT)
   ```
   - 只有当前一个步骤的返回码大于 2 时，才会执行当前步骤。

6. **`GE` (Greater Than or Equal)**  
   表示前一个步骤的返回码大于或等于指定的值。

   ```jcl
   //COND COND=(5,GE)
   ```
   - 只有当前一个步骤的返回码大于或等于 5 时，才会执行当前步骤。

#### 语法说明

`COND` 语句的基本格式如下：

```jcl
//stepname EXEC PGM=yourprogram,COND=(return_code,operator)
```

- **return_code**：前一个步骤的返回码（例如 0, 8, 4 等）。
- **operator**：比较运算符，可以是 `EQ`, `NE`, `LT`, `LE`, `GT`, `GE`。

#### 多条件判断

`COND` 语句还支持多个条件的组合，使用逗号分隔，支持逻辑 "OR"。

```jcl
//COND COND=(return_code1,operator1),(return_code2,operator2)
```

例如：

```jcl
//COND COND=(4,LE),(8,GT)
```

表示：前一个步骤的返回码小于或等于 4，或者大于 8 时，才会执行当前步骤。

#### 总结

- **EQ**：前一个步骤返回码等于指定值。
- **NE**：前一个步骤返回码不等于指定值。
- **LT**：前一个步骤返回码小于指定值。
- **LE**：前一个步骤返回码小于或等于指定值。
- **GT**：前一个步骤返回码大于指定值。
- **GE**：前一个步骤返回码大于或等于指定值。

这些操作符可以帮助你根据不同的返回码来灵活控制作业步骤的执行。