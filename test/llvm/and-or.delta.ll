
declare i1 @a()

declare i1 @b()

declare i1 @c()

define i32 @main() {
  %and = alloca i1
  %and2 = alloca i1
  %or = alloca i1
  %or2 = alloca i1
  %1 = call i1 @a()
  br i1 %1, label %and.rhs, label %and.end

and.rhs:                                          ; preds = %0
  %2 = call i1 @b()
  br label %and.end

and.end:                                          ; preds = %and.rhs, %0
  %and1 = phi i1 [ %1, %0 ], [ %2, %and.rhs ]
  store i1 %and1, i1* %and
  %3 = call i1 @a()
  br i1 %3, label %and.rhs3, label %and.end4

and.rhs2:                                         ; preds = %and.end4
  %4 = call i1 @c()
  br label %and.end6

and.rhs3:                                         ; preds = %and.end
  %5 = call i1 @b()
  br label %and.end4

and.end4:                                         ; preds = %and.rhs3, %and.end
  %and5 = phi i1 [ %3, %and.end ], [ %5, %and.rhs3 ]
  br i1 %and5, label %and.rhs2, label %and.end6

and.end6:                                         ; preds = %and.end4, %and.rhs2
  %and7 = phi i1 [ %and5, %and.end4 ], [ %4, %and.rhs2 ]
  store i1 %and7, i1* %and2
  %6 = call i1 @a()
  br i1 %6, label %or.end, label %or.rhs

or.rhs:                                           ; preds = %and.end6
  %7 = call i1 @b()
  br label %or.end

or.end:                                           ; preds = %or.rhs, %and.end6
  %or8 = phi i1 [ %6, %and.end6 ], [ %7, %or.rhs ]
  store i1 %or8, i1* %or
  %8 = call i1 @a()
  br i1 %8, label %or.end11, label %or.rhs10

or.rhs9:                                          ; preds = %or.end11
  %9 = call i1 @c()
  br label %or.end13

or.rhs10:                                         ; preds = %or.end
  %10 = call i1 @b()
  br label %or.end11

or.end11:                                         ; preds = %or.rhs10, %or.end
  %or12 = phi i1 [ %8, %or.end ], [ %10, %or.rhs10 ]
  br i1 %or12, label %or.end13, label %or.rhs9

or.end13:                                         ; preds = %or.end11, %or.rhs9
  %or14 = phi i1 [ %or12, %or.end11 ], [ %9, %or.rhs9 ]
  store i1 %or14, i1* %or2
  ret i32 0
}
