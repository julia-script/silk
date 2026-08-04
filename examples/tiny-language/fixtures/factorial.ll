; ModuleID = 'tiny-language'
source_filename = "program.tiny"

define i32 @factorial(i32 %v0) {
entry:
  %comparison_0 = icmp slt i32 %v0, 2
  %comparison_i32_1 = zext i1 %comparison_0 to i32
  %condition_2 = icmp ne i32 %comparison_i32_1, 0
  br i1 %condition_2, label %if_true_0, label %if_false_0
if_true_0:
  br label %if_merge_0
if_false_0:
  %subtracted_3 = sub i32 %v0, 1
  %called_4 = call i32 @factorial(i32 %subtracted_3)
  %multiplied_5 = mul i32 %v0, %called_4
  br label %if_merge_0
if_merge_0:
  %if_result_6 = phi i32 [ 1, %if_true_0 ], [ %multiplied_5, %if_false_0 ]
  ret i32 %if_result_6
}
define i32 @main() {
entry:
  %called_0 = call i32 @factorial(i32 5)
  ret i32 %called_0
}
