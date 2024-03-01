;! @.str = global [4 x i8] c"%f\0A\00", align 1
;! declare i32 @printf(ptr , ...) 

; Function Attrs: noinline nounwind optnone uwtable


define float @llvm.fmuladd.f32(float %a, float %b, float %c){
  %d = fmul float %a, %b 
  %r = fadd float %d, %c
  ret float %r
}


define  float @custom_atof(ptr  %0)  {
  %2 = alloca ptr, align 8
  %3 = alloca float, align 4
  %4 = alloca i32, align 4
  %5 = alloca i32, align 4
  %6 = alloca float, align 4
  store ptr %0, ptr %2, align 8
  store float 0.000000e+00, ptr %3, align 4
  store i32 0, ptr %4, align 4
  store i32 1, ptr %5, align 4
  store float 1.000000e+00, ptr %6, align 4
  %7 = load ptr, ptr %2, align 8
  %8 = getelementptr  i8, ptr %7, i64 0
  %9 = load i8, ptr %8, align 1
  %10 = sext i8 %9 to i32
  %11 = icmp eq i32 %10, 45
  br i1 %11, label %12, label %15

12:                                               ; preds = %1
  store i32 -1, ptr %5, align 4
  %13 = load i32, ptr %4, align 4
  %14 = add nsw i32 %13, 1
  store i32 %14, ptr %4, align 4
  br label %15

15:                                               ; preds = %12, %1
  br label %16

16:                                               ; preds = %45, %15
  %17 = load ptr, ptr %2, align 8
  %18 = load i32, ptr %4, align 4
  %19 = sext i32 %18 to i64
  %20 = getelementptr  i8, ptr %17, i64 %19
  %21 = load i8, ptr %20, align 1
  %22 = sext i8 %21 to i32
  %23 = icmp ne i32 %22, 46
  br i1 %23, label %24, label %32

24:                                               ; preds = %16
  %25 = load ptr, ptr %2, align 8
  %26 = load i32, ptr %4, align 4
  %27 = sext i32 %26 to i64
  %28 = getelementptr  i8, ptr %25, i64 %27
  %29 = load i8, ptr %28, align 1
  %30 = sext i8 %29 to i32
  %31 = icmp ne i32 %30, 0
  br label %32

32:                                               ; preds = %24, %16
  %33 = phi i1 [ false, %16 ], [ %31, %24 ]
  br i1 %33, label %34, label %48

34:                                               ; preds = %32
  %35 = load float, ptr %3, align 4
  %36 = load ptr, ptr %2, align 8
  %37 = load i32, ptr %4, align 4
  %38 = sext i32 %37 to i64
  %39 = getelementptr  i8, ptr %36, i64 %38
  %40 = load i8, ptr %39, align 1
  %41 = sext i8 %40 to i32
  %42 = sub nsw i32 %41, 48
  %43 = sitofp i32 %42 to float
  %44 = call float @llvm.fmuladd.f32(float %35, float 1.000000e+01, float %43)
  store float %44, ptr %3, align 4
  br label %45

45:                                               ; preds = %34
  %46 = load i32, ptr %4, align 4
  %47 = add nsw i32 %46, 1
  store i32 %47, ptr %4, align 4
  br label %16

48:                                               ; preds = %32
  %49 = load ptr, ptr %2, align 8
  %50 = load i32, ptr %4, align 4
  %51 = sext i32 %50 to i64
  %52 = getelementptr  i8, ptr %49, i64 %51
  %53 = load i8, ptr %52, align 1
  %54 = sext i8 %53 to i32
  %55 = icmp eq i32 %54, 46
  br i1 %55, label %56, label %59

56:                                               ; preds = %48
  %57 = load i32, ptr %4, align 4
  %58 = add nsw i32 %57, 1
  store i32 %58, ptr %4, align 4
  br label %59

59:                                               ; preds = %56, %48
  br label %60

60:                                               ; preds = %81, %59
  %61 = load ptr, ptr %2, align 8
  %62 = load i32, ptr %4, align 4
  %63 = sext i32 %62 to i64
  %64 = getelementptr  i8, ptr %61, i64 %63
  %65 = load i8, ptr %64, align 1
  %66 = sext i8 %65 to i32
  %67 = icmp ne i32 %66, 0
  br i1 %67, label %68, label %84

68:                                               ; preds = %60
  %69 = load float, ptr %3, align 4
  %70 = load ptr, ptr %2, align 8
  %71 = load i32, ptr %4, align 4
  %72 = sext i32 %71 to i64
  %73 = getelementptr  i8, ptr %70, i64 %72
  %74 = load i8, ptr %73, align 1
  %75 = sext i8 %74 to i32
  %76 = sub nsw i32 %75, 48
  %77 = sitofp i32 %76 to float
  %78 = call float @llvm.fmuladd.f32(float %69, float 1.000000e+01, float %77)
  store float %78, ptr %3, align 4
  %79 = load float, ptr %6, align 4
  %80 = fmul float %79, 1.000000e+01
  store float %80, ptr %6, align 4
  br label %81

81:                                               ; preds = %68
  %82 = load i32, ptr %4, align 4
  %83 = add nsw i32 %82, 1
  store i32 %83, ptr %4, align 4
  br label %60

84:                                               ; preds = %60
  %85 = load i32, ptr %5, align 4
  %86 = sitofp i32 %85 to float
  %87 = load float, ptr %3, align 4
  %88 = fmul float %86, %87
  %89 = load float, ptr %6, align 4
  %90 = fdiv float %88, %89
  ret float %90
}


; Function Attrs: noinline nounwind optnone uwtable
define  float @my_fabs(float  %0)  {
  %2 = alloca float, align 4
  store float %0, ptr %2, align 4
  %3 = load float, ptr %2, align 4
  %4 = fcmp olt float %3, 0.000000e+00
  br i1 %4, label %5, label %8

5:                                                ; preds = %1
  %6 = load float, ptr %2, align 4
  %7 = fneg float %6
  store float %7, ptr %2, align 4
  br label %8

8:                                                ; preds = %5, %1
  %9 = load float, ptr %2, align 4
  ret float %9
}

; Function Attrs: noinline nounwind optnone uwtable
define  float @findArea(<2 x float> %0, <2 x float> %1, <2 x float> %2)  {
  %4 = alloca{float, float}, align 4
  %5 = alloca{float, float}, align 4
  %6 = alloca{float, float}, align 4
  store <2 x float> %0, ptr %4, align 4
  store <2 x float> %1, ptr %5, align 4
  store <2 x float> %2, ptr %6, align 4
  %7 = getelementptr {float, float}, ptr %4, i32 0, i32 0
  %8 = load float, ptr %7, align 4
  %9 = getelementptr {float, float}, ptr %5, i32 0, i32 1
  %10 = load float, ptr %9, align 4
  %11 = getelementptr {float, float}, ptr %6, i32 0, i32 1
  %12 = load float, ptr %11, align 4
  %13 = fsub float %10, %12
  %14 = getelementptr {float, float}, ptr %5, i32 0, i32 0
  %15 = load float, ptr %14, align 4
  %16 = getelementptr {float, float}, ptr %6, i32 0, i32 1
  %17 = load float, ptr %16, align 4
  %18 = getelementptr {float, float}, ptr %4, i32 0, i32 1
  %19 = load float, ptr %18, align 4
  %20 = fsub float %17, %19
  %21 = fmul float %15, %20
  %22 = call float @llvm.fmuladd.f32(float %8, float %13, float %21)
  %23 = getelementptr {float, float}, ptr %6, i32 0, i32 0
  %24 = load float, ptr %23, align 4
  %25 = getelementptr {float, float}, ptr %4, i32 0, i32 1
  %26 = load float, ptr %25, align 4
  %27 = getelementptr {float, float}, ptr %5, i32 0, i32 1
  %28 = load float, ptr %27, align 4
  %29 = fsub float %26, %28
  %30 = call float @llvm.fmuladd.f32(float %24, float %29, float %22)
  %31 = fdiv float %30, 2.000000e+00
  %32 = call float @my_fabs(float  %31)
  ret float %32
}

; Function Attrs: noinline nounwind optnone uwtable
define  float @main(i32  %0, ptr  %1)  {
  %3 = alloca i32, align 4
  %4 = alloca ptr, align 8
  %5 = alloca [3 x{float, float}], align 16
  %6 = alloca i32, align 4
  %7 = alloca float, align 4
  %8 = alloca float, align 4
  %9 = alloca float, align 4
  store i32 %0, ptr %3, align 4
  store ptr %1, ptr %4, align 8
  %10 = load i32, ptr %3, align 4
  %11 = icmp sgt i32 %10, 6
  br i1 %11, label %12, label %47

12:                                               ; preds = %2
  store i32 0, ptr %6, align 4
  br label %13

13:                                               ; preds = %43, %12
  %14 = load i32, ptr %6, align 4
  %15 = icmp slt i32 %14, 3
  br i1 %15, label %16, label %46

16:                                               ; preds = %13
  %17 = load ptr, ptr %4, align 8
  %18 = load i32, ptr %6, align 4
  %19 = mul nsw i32 %18, 2
  %20 = add nsw i32 1, %19
  %21 = sext i32 %20 to i64
  %22 = getelementptr  ptr, ptr %17, i64 %21
  %23 = load ptr, ptr %22, align 8
  %24 = call float @custom_atof(ptr  %23)
  store float %24, ptr %7, align 4
  %25 = load ptr, ptr %4, align 8
  %26 = load i32, ptr %6, align 4
  %27 = mul nsw i32 %26, 2
  %28 = add nsw i32 %27, 2
  %29 = sext i32 %28 to i64
  %30 = getelementptr  ptr, ptr %25, i64 %29
  %31 = load ptr, ptr %30, align 8
  %32 = call float @custom_atof(ptr  %31)
  store float %32, ptr %8, align 4
  %33 = load float, ptr %7, align 4
  %34 = load i32, ptr %6, align 4
  %35 = sext i32 %34 to i64
  %36 = getelementptr  [3 x{float, float}], ptr %5, i64 0, i64 %35
  %37 = getelementptr {float, float}, ptr %36, i32 0, i32 0
  store float %33, ptr %37, align 8
  %38 = load float, ptr %8, align 4
  %39 = load i32, ptr %6, align 4
  %40 = sext i32 %39 to i64
  %41 = getelementptr  [3 x{float, float}], ptr %5, i64 0, i64 %40
  %42 = getelementptr {float, float}, ptr %41, i32 0, i32 1
  store float %38, ptr %42, align 4
  br label %43

43:                                               ; preds = %16
  %44 = load i32, ptr %6, align 4
  %45 = add nsw i32 %44, 1
  store i32 %45, ptr %6, align 4
  br label %13

46:                                               ; preds = %13
  br label %47

47:                                               ; preds = %46, %2
  %48 = getelementptr  [3 x{float, float}], ptr %5, i64 0, i64 0
  %49 = getelementptr  [3 x{float, float}], ptr %5, i64 0, i64 1
  %50 = getelementptr  [3 x{float, float}], ptr %5, i64 0, i64 2
  %51 = load <2 x float>, ptr %48, align 16
  %52 = load <2 x float>, ptr %49, align 8
  %53 = load <2 x float>, ptr %50, align 16
  %54 = call float @findArea(<2 x float> %51, <2 x float> %52, <2 x float> %53)
  store float %54, ptr %9, align 4
  %55 = load float, ptr %9, align 4
  ;! %fd = fpext float %55 to double
  ;! %print_res = call i32 (ptr, ...) @printf(ptr @.str, double %fd)
  ret float %55
}
