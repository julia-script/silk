; ModuleID = 'tiny-language'
source_filename = "program.tiny"

define i32 @main() {
entry:
  %condition_0 = icmp ne i32 1, 0
  br i1 %condition_0, label %if_true_0, label %if_false_0
if_true_0:
  %condition_1 = icmp ne i32 0, 0
  br i1 %condition_1, label %if_true_1, label %if_false_1
if_false_0:
  br label %if_merge_0
if_merge_0:
  %if_result_3 = phi i32 [ %if_result_2, %if_merge_1 ], [ 4, %if_false_0 ]
  ret i32 %if_result_3
if_true_1:
  br label %if_merge_1
if_false_1:
  br label %if_merge_1
if_merge_1:
  %if_result_2 = phi i32 [ 2, %if_true_1 ], [ 3, %if_false_1 ]
  br label %if_merge_0
}
