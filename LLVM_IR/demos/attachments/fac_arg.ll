;! @.str = global [4 x i8] c"%d\0A\00", align 1
;! declare i32 @printf(ptr , ...) 

; Function Attrs: noinline nounwind optnone uwtable
define  i32 @char2int(ptr  %0, i32  %1)  {
  %3 = alloca ptr, align 8
  %4 = alloca i32, align 4
  %5 = alloca i32, align 4
  %6 = alloca i32, align 4
  store ptr %0, ptr %3, align 8
  store i32 %1, ptr %4, align 4
  store i32 0, ptr %5, align 4
  store i32 1, ptr %6, align 4
  %7 = load i32, ptr %4, align 4
  %8 = icmp slt i32 %7, 0
  br i1 %8, label %9, label %12

9:                                                ; preds = %2
  %10 = load i32, ptr %4, align 4
  %11 = sub nsw i32 0, %10
  br label %14

12:                                               ; preds = %2
  %13 = load i32, ptr %4, align 4
  br label %14

14:                                               ; preds = %12, %9
  %15 = phi i32 [ %11, %9 ], [ %13, %12 ]
  store i32 %15, ptr %4, align 4
  br label %16

16:                                               ; preds = %78, %48, %14
  %17 = load i32, ptr %4, align 4
  %18 = add nsw i32 %17, -1
  store i32 %18, ptr %4, align 4
  %19 = icmp ne i32 %17, 0
  br i1 %19, label %20, label %79

20:                                               ; preds = %16
  %21 = load ptr, ptr %3, align 8
  %22 = load i32, ptr %4, align 4
  %23 = sext i32 %22 to i64
  %24 = getelementptr  i8, ptr %21, i64 %23
  %25 = load i8, ptr %24, align 1
  %26 = sext i8 %25 to i32
  %27 = icmp slt i32 %26, 48
  br i1 %27, label %36, label %28

28:                                               ; preds = %20
  %29 = load ptr, ptr %3, align 8
  %30 = load i32, ptr %4, align 4
  %31 = sext i32 %30 to i64
  %32 = getelementptr  i8, ptr %29, i64 %31
  %33 = load i8, ptr %32, align 1
  %34 = sext i8 %33 to i32
  %35 = icmp sgt i32 %34, 57
  br i1 %35, label %36, label %49

36:                                               ; preds = %28, %20
  %37 = load ptr, ptr %3, align 8
  %38 = load i32, ptr %4, align 4
  %39 = sext i32 %38 to i64
  %40 = getelementptr  i8, ptr %37, i64 %39
  %41 = load i8, ptr %40, align 1
  %42 = sext i8 %41 to i32
  %43 = icmp ne i32 %42, 45
  br i1 %43, label %44, label %49

44:                                               ; preds = %36
  %45 = load i32, ptr %5, align 4
  %46 = icmp ne i32 %45, 0
  br i1 %46, label %47, label %48

47:                                               ; preds = %44
  br label %79

48:                                               ; preds = %44
  br label %16

49:                                               ; preds = %36, %28
  %50 = load ptr, ptr %3, align 8
  %51 = load i32, ptr %4, align 4
  %52 = sext i32 %51 to i64
  %53 = getelementptr  i8, ptr %50, i64 %52
  %54 = load i8, ptr %53, align 1
  %55 = sext i8 %54 to i32
  %56 = icmp eq i32 %55, 45
  br i1 %56, label %57, label %64

57:                                               ; preds = %49
  %58 = load i32, ptr %5, align 4
  %59 = icmp ne i32 %58, 0
  br i1 %59, label %60, label %63

60:                                               ; preds = %57
  %61 = load i32, ptr %5, align 4
  %62 = sub nsw i32 0, %61
  store i32 %62, ptr %5, align 4
  br label %79

63:                                               ; preds = %57
  br label %78

64:                                               ; preds = %49
  %65 = load ptr, ptr %3, align 8
  %66 = load i32, ptr %4, align 4
  %67 = sext i32 %66 to i64
  %68 = getelementptr  i8, ptr %65, i64 %67
  %69 = load i8, ptr %68, align 1
  %70 = sext i8 %69 to i32
  %71 = sub nsw i32 %70, 48
  %72 = load i32, ptr %6, align 4
  %73 = mul nsw i32 %71, %72
  %74 = load i32, ptr %5, align 4
  %75 = add nsw i32 %74, %73
  store i32 %75, ptr %5, align 4
  %76 = load i32, ptr %6, align 4
  %77 = mul nsw i32 %76, 10
  store i32 %77, ptr %6, align 4
  br label %78

78:                                               ; preds = %64, %63
  br label %16

79:                                               ; preds = %60, %47, %16
  %80 = load i32, ptr %5, align 4
  ret i32 %80
}

; Function Attrs: noinline nounwind optnone uwtable
define  i32 @mystrlen(ptr  %0)  {
  %2 = alloca ptr, align 8
  %3 = alloca i32, align 4
  store ptr %0, ptr %2, align 8
  store i32 0, ptr %3, align 4
  br label %4

4:                                                ; preds = %12, %1
  %5 = load ptr, ptr %2, align 8
  %6 = load i32, ptr %3, align 4
  %7 = add nsw i32 %6, 1
  store i32 %7, ptr %3, align 4
  %8 = sext i32 %6 to i64
  %9 = getelementptr  i8, ptr %5, i64 %8
  %10 = load i8, ptr %9, align 1
  %11 = icmp ne i8 %10, 0
  br i1 %11, label %12, label %13

12:                                               ; preds = %4
  br label %4

13:                                               ; preds = %4
  %14 = load i32, ptr %3, align 4
  ret i32 %14
}

; Function Attrs: noinline nounwind optnone uwtable
define  i32 @fac(i32  %0)  {
  %2 = alloca i32, align 4
  %3 = alloca i32, align 4
  store i32 %0, ptr %3, align 4
  %4 = load i32, ptr %3, align 4
  %5 = icmp slt i32 %4, 1
  br i1 %5, label %6, label %7

6:                                                ; preds = %1
  store i32 1, ptr %2, align 4
  br label %13

7:                                                ; preds = %1
  %8 = load i32, ptr %3, align 4
  %9 = sub nsw i32 %8, 1
  %10 = call i32 @fac(i32  %9)
  %11 = load i32, ptr %3, align 4
  %12 = mul nsw i32 %10, %11
  store i32 %12, ptr %2, align 4
  br label %13

13:                                               ; preds = %7, %6
  %14 = load i32, ptr %2, align 4
  ret i32 %14
}

; Function Attrs: noinline nounwind optnone uwtable
define i32 @main(i32  %0, ptr  %1) {
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  %5 = alloca ptr, align 8
  %6 = alloca ptr, align 8
  %7 = alloca i32, align 4
  %8 = alloca i32, align 4
  store i32 0, ptr %3, align 4
  store i32 %0, ptr %4, align 4
  store ptr %1, ptr %5, align 8
  %9 = load i32, ptr %4, align 4
  %10 = icmp sgt i32 %9, 1
  br i1 %10, label %11, label %22

11:                                               ; preds = %2
  %12 = load ptr, ptr %5, align 8
  %13 = getelementptr ptr, ptr %12, i64 1
  %14 = load ptr, ptr %13, align 8
  store ptr %14, ptr %6, align 8
  %15 = load ptr, ptr %6, align 8
  %16 = call i32 @mystrlen(ptr  %15)
  store i32 %16, ptr %7, align 4
  %17 = load ptr, ptr %6, align 8
  %18 = load i32, ptr %7, align 4
  %19 = call i32 @char2int(ptr  %17, i32  %18)
  store i32 %19, ptr %8, align 4
  %20 = load i32, ptr %8, align 4
  %21 = call i32 @fac(i32  %20)
  store i32 %21, ptr %3, align 4
  br label %23

22:                                               ; preds = %2
  store i32 0, ptr %3, align 4
  br label %23

23:                                               ; preds = %22, %11
  %24 = load i32, ptr %3, align 4
  ;! %print_res = call i32 (ptr, ...) @printf(ptr @.str, i32 %24)
  ;! ret i32 0
  ret i32 %24
}