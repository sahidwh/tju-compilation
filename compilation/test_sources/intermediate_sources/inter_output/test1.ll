; ModuleID = 'sysy2022_compiler'
source_filename = "/home/musashi/compilation/test_sources/intermediate_sources/inter_test/test1.sy"
declare i32 @getint()

declare i32 @getch()

declare i32 @getarray(i32*)

declare void @putint(i32)

declare void @putch(i32)

declare void @putarray(i32, i32*)

declare void @starttime()

declare void @stoptime()

define i32 @main() {
label_main.entry:
  %op0 = alloca i32
  store i32 1, i32* %op0
  %op1 = alloca i32
  store i32 2, i32* %op1
  %op2 = load i32, i32* %op0
  %op3 = load i32, i32* %op1
  %op4 = add i32 %op2, %op3
  %op5 = sub i32 %op4, 1
  store i32 %op5, i32* %op0
  %op6 = load i32, i32* %op0
  %op7 = icmp eq i32 %op6, 2
  %op8 = zext i1 %op7 to i32
  %op9 = icmp ne i32 %op8, 0
  br i1 %op9, label %label_if.then.0, label %label_if.else.2
label_if.then.0:                                                ; preds = %label_main.entry
  ret i32 0
label_if.end.1:
  ret i32 0
label_if.else.2:                                                ; preds = %label_main.entry
  %op10 = load i32, i32* %op1
  %op11 = mul i32 %op10, 2
  %op12 = sdiv i32 %op11, 1
  %op13 = srem i32 %op12, 2
  store i32 %op13, i32* %op1
  ret i32 1
}
