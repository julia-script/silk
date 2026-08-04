; ModuleID = 'tiny-language'
source_filename = "program.tiny"

define i32 @abs(i32 %v0) {
entry:
  %comparison_0 = icmp slt i32 %v0, 0
  %comparison_i32_1 = zext i1 %comparison_0 to i32
  %condition_2 = icmp ne i32 %comparison_i32_1, 0
  br i1 %condition_2, label %if_true_0, label %if_false_0
if_true_0:
  %negated_3 = sub i32 zeroinitializer, %v0
  br label %if_merge_0
if_false_0:
  br label %if_merge_0
if_merge_0:
  %if_result_4 = phi i32 [ %negated_3, %if_true_0 ], [ %v0, %if_false_0 ]
  ret i32 %if_result_4
}
define i32 @main() {
entry:
  %negated_0 = sub i32 zeroinitializer, 3
  %called_1 = call i32 @abs(i32 %negated_0)
  ret i32 %called_1
}
