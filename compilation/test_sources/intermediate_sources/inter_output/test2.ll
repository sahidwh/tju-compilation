; ModuleID = 'sysy2022_compiler'
source_filename = "/home/musashi/compilation/test_sources/intermediate_sources/inter_test/test2.sy"
declare i32 @getint()

declare i32 @getch()

declare i32 @getarray(i32*)

declare void @putint(i32)

declare void @putch(i32)

declare void @putarray(i32, i32*)

declare void @starttime()

declare void @stoptime()

define void @main() {
label_main.entry:
  %op0 = alloca i32
  store i32 10, i32* %op0
  %op1 = alloca i32
  store i32 2, i32* %op1
  %op2 = alloca i32
  store i32 0, i32* %op2
  %op3 = alloca i32
  store i32 1, i32* %op3
  %op4 = load i32, i32* %op2
  %op5 = load i32, i32* %op0
  %op6 = add i32 %op4, %op5
  %op7 = load i32, i32* %op1
  %op8 = mul i32 %op7, 2
  %op9 = sdiv i32 %op8, 1
  %op10 = srem i32 %op9, 2
  %op11 = sub i32 %op6, %op10
  store i32 %op11, i32* %op2
  %op12 = load i32, i32* %op2
  %op13 = load i32, i32* %op3
  %op14 = icmp sge i32 %op12, %op13
  %op15 = zext i1 %op14 to i32
  %op16 = icmp ne i32 %op15, 0
  br i1 %op16, label %label_land.next.2, label %label_land.merge.1
label_lor.merge.0:                                                ; preds = %label_land.merge.1, %label_lor.next.4, %label_lor.false.5
  %op17 = zext i1 % to i32
  %op18 = icmp ne i32 %op17, 0
  br i1 %op18, label %label_if.then.6, label %label_if.else.8
label_land.merge.1:                                                ; preds = %label_main.entry, %label_land.next.2, %label_land.true.3
  %op19 = zext i1 % to i32
  %op20 = icmp ne i32 %op19, 0
  br i1 %op20, label %label_lor.merge.0, label %label_lor.next.4
label_land.next.2:                                                ; preds = %label_main.entry
  %op21 = icmp ne i32 0, 0
  %op22 = zext i1 %op21 to i32
  %op23 = icmp ne i32 %op22, 0
  br i1 %op23, label %label_land.true.3, label %label_land.merge.1
label_land.true.3:                                                ; preds = %label_land.next.2
  br label %label_land.merge.1
label_lor.next.4:                                                ; preds = %label_land.merge.1
  %op24 = load i32, i32* %op0
  %op25 = load i32, i32* %op1
  %op26 = icmp slt i32 %op24, %op25
  %op27 = zext i1 %op26 to i32
  %op28 = icmp ne i32 %op27, 0
  br i1 %op28, label %label_lor.merge.0, label %label_lor.false.5
label_lor.false.5:                                                ; preds = %label_lor.next.4
  br label %label_lor.merge.0
label_if.then.6:                                                ; preds = %label_lor.merge.0
  %op29 = load i32, i32* %op3
  %op30 = add i32 %op29, 1
  store i32 %op30, i32* %op3
  br label %label_if.end.7
label_if.end.7:                                                ; preds = %label_if.then.6, %label_if.else.8
  %op31 = load i32, i32* %op2
  %op32 = load i32, i32* %op3
  %op33 = icmp sgt i32 %op31, %op32
  %op34 = zext i1 %op33 to i32
  store i32 %op34, i32* %op2
  %op35 = load i32, i32* %op2
  %op36 = load i32, i32* %op3
  %op37 = icmp slt i32 %op35, %op36
  %op38 = zext i1 %op37 to i32
  store i32 %op38, i32* %op2
  %op39 = load i32, i32* %op2
  %op40 = load i32, i32* %op3
  %op41 = icmp eq i32 %op39, %op40
  %op42 = zext i1 %op41 to i32
  store i32 %op42, i32* %op2
  %op43 = load i32, i32* %op2
  %op44 = load i32, i32* %op3
  %op45 = icmp sle i32 %op43, %op44
  %op46 = zext i1 %op45 to i32
  store i32 %op46, i32* %op2
  %op47 = load i32, i32* %op2
  %op48 = load i32, i32* %op3
  %op49 = icmp sge i32 %op47, %op48
  %op50 = zext i1 %op49 to i32
  store i32 %op50, i32* %op2
  ret void
label_if.else.8:                                                ; preds = %label_lor.merge.0
  %op51 = load i32, i32* %op3
  %op52 = sub i32 %op51, 1
  store i32 %op52, i32* %op3
  br label %label_if.end.7
}
