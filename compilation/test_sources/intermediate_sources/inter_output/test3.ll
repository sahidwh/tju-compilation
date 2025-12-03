; ModuleID = 'sysy2022_compiler'
source_filename = "/home/musashi/compilation/test_sources/intermediate_sources/inter_test/test3.sy"
declare i32 @getint()

declare i32 @getch()

declare i32 @getarray(i32*)

declare void @putint(i32)

declare void @putch(i32)

declare void @putarray(i32, i32*)

declare void @starttime()

declare void @stoptime()

define void @MaIn() {
label_MaIn.entry:
  %op0 = alloca i32
  store i32 42, i32* %op0
  %op1 = alloca i32
  store i32 0, i32* %op1
  %op2 = alloca i32
  store i32 1, i32* %op2
  %op3 = alloca i32
  store i32 2, i32* %op3
  %op4 = load i32, i32* %op1
  %op5 = load i32, i32* %op0
  %op6 = mul i32 %op5, 2
  %op7 = add i32 %op4, %op6
  %op8 = sdiv i32 5, 3
  %op9 = srem i32 %op8, 2
  %op10 = sub i32 %op7, %op9
  store i32 %op10, i32* %op1
  %op11 = load i32, i32* %op2
  %op12 = load i32, i32* %op3
  %op13 = add i32 %op11, %op12
  store i32 %op13, i32* %op2
  %op14 = load i32, i32* %op1
  %op15 = icmp sge i32 %op14, 10
  %op16 = zext i1 %op15 to i32
  %op17 = icmp ne i32 %op16, 0
  br i1 %op17, label %label_land.next.2, label %label_land.merge.1
label_lor.merge.0:                                                ; preds = %label_land.merge.1, %label_lor.next.4, %label_lor.false.5
  %op18 = zext i1 % to i32
  %op19 = icmp ne i32 %op18, 0
  br i1 %op19, label %label_if.then.6, label %label_if.else.8
label_land.merge.1:                                                ; preds = %label_MaIn.entry, %label_land.next.2, %label_land.true.3
  %op20 = zext i1 % to i32
  %op21 = icmp ne i32 %op20, 0
  br i1 %op21, label %label_lor.merge.0, label %label_lor.next.4
label_land.next.2:                                                ; preds = %label_MaIn.entry
  %op22 = load i32, i32* %op1
  %op23 = icmp ne i32 %op22, 0
  %op24 = zext i1 %op23 to i32
  %op25 = icmp ne i32 %op24, 0
  br i1 %op25, label %label_land.true.3, label %label_land.merge.1
label_land.true.3:                                                ; preds = %label_land.next.2
  br label %label_land.merge.1
label_lor.next.4:                                                ; preds = %label_land.merge.1
  %op26 = load i32, i32* %op2
  %op27 = load i32, i32* %op3
  %op28 = icmp slt i32 %op26, %op27
  %op29 = zext i1 %op28 to i32
  %op30 = icmp ne i32 %op29, 0
  br i1 %op30, label %label_lor.merge.0, label %label_lor.false.5
label_lor.false.5:                                                ; preds = %label_lor.next.4
  br label %label_lor.merge.0
label_if.then.6:                                                ; preds = %label_lor.merge.0
  %op31 = load i32, i32* %op1
  %op32 = add i32 %op31, 1
  store i32 %op32, i32* %op1
  br label %label_if.end.7
label_if.end.7:                                                ; preds = %label_if.then.6, %label_if.else.8
  %op33 = icmp sgt i32 1, 1
  %op34 = zext i1 %op33 to i32
  %op35 = icmp ne i32 %op34, 0
  br i1 %op35, label %label_land.next.10, label %label_land.merge.9
label_if.else.8:                                                ; preds = %label_lor.merge.0
  %op36 = load i32, i32* %op1
  %op37 = sub i32 %op36, 1
  store i32 %op37, i32* %op1
  br label %label_if.end.7
label_land.merge.9:                                                ; preds = %label_if.end.7, %label_land.next.10, %label_land.true.11
  %op38 = zext i1 % to i32
  %op39 = icmp ne i32 %op38, 0
  br i1 %op39, label %label_if.then.12, label %label_if.else.14
label_land.next.10:                                                ; preds = %label_if.end.7
  %op40 = icmp sle i32 0, 1
  %op41 = zext i1 %op40 to i32
  %op42 = icmp ne i32 %op41, 0
  br i1 %op42, label %label_land.true.11, label %label_land.merge.9
label_land.true.11:                                                ; preds = %label_land.next.10
  br label %label_land.merge.9
label_if.then.12:                                                ; preds = %label_land.merge.9
  %op43 = load i32, i32* %op2
  %op44 = load i32, i32* %op3
  %op45 = add i32 %op43, %op44
  %op46 = load i32, i32* %op0
  %op47 = sub i32 %op46, 3
  %op48 = mul i32 %op45, %op47
  store i32 %op48, i32* %op2
  br label %label_if.end.13
label_if.end.13:                                                ; preds = %label_if.then.12, %label_if.end.16
  ret void
label_if.else.14:                                                ; preds = %label_land.merge.9
  %op49 = load i32, i32* %op2
  %op50 = load i32, i32* %op3
  %op51 = icmp eq i32 %op49, %op50
  %op52 = zext i1 %op51 to i32
  %op53 = icmp ne i32 %op52, 0
  br i1 %op53, label %label_if.then.15, label %label_if.else.17
label_if.then.15:                                                ; preds = %label_if.else.14
  %op54 = load i32, i32* %op3
  %op55 = add i32 %op54, 1
  store i32 %op55, i32* %op3
  br label %label_if.end.16
label_if.end.16:                                                ; preds = %label_if.then.15, %label_if.else.17
  br label %label_if.end.13
label_if.else.17:                                                ; preds = %label_if.else.14
  %op56 = load i32, i32* %op3
  %op57 = sub i32 %op56, 1
  store i32 %op57, i32* %op3
  br label %label_if.end.16
}
