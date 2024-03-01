;! @.str = global [4 x i8] c"%d\0A\00", align 1
;! declare i32 @printf(ptr , ...) 

; Function Attrs: noinline nounwind optnone uwtable
define i32 @char2int(ptr %c0, i32 %c1) {
  %c3 = alloca ptr, align 8
  %c4 = alloca i32, align 4
  %c5 = alloca i32, align 4
  %c6 = alloca i32, align 4
  store ptr %c0, ptr %c3, align 8
  store i32 %c1, ptr %c4, align 4
  store i32 0, ptr %c5, align 4
  store i32 1, ptr %c6, align 4
  %c7 = load i32, ptr %c4, align 4
  %c8 = icmp slt i32 %c7, 0
  br i1 %c8, label %c9, label %c12

c9:                                                ; preds = %2
  %c10 = load i32, ptr %c4, align 4
  %c11 = sub nsw i32 0, %c10
  br label %c14

c12:                                               ; preds = %2
  %c13 = load i32, ptr %c4, align 4
  br label %c14

c14:                                               ; preds = %c12, %9
  %c15 = phi i32 [ %c11, %c9 ], [ %c13, %c12 ]
  store i32 %c15, ptr %c4, align 4
  br label %c16

c16:                                               ; preds = %c78, %c48, %c14
  %c17 = load i32, ptr %c4, align 4
  %c18 = add nsw i32 %c17, -1
  store i32 %c18, ptr %c4, align 4
  %c19 = icmp ne i32 %c17, 0
  br i1 %c19, label %c20, label %c79

c20:                                               ; preds = %c16
  %c21 = load ptr, ptr %c3, align 8
  %c22 = load i32, ptr %c4, align 4
  %c23 = sext i32 %c22 to i64
  %c24 = getelementptr i8, ptr %c21, i64 %c23
  %c25 = load i8, ptr %c24, align 1
  %c26 = sext i8 %c25 to i32
  %c27 = icmp slt i32 %c26, 48
  br i1 %c27, label %c36, label %c28

c28:                                               ; preds = %c20
  %c29 = load ptr, ptr %c3, align 8
  %c30 = load i32, ptr %c4, align 4
  %c31 = sext i32 %c30 to i64
  %c32 = getelementptr i8, ptr %c29, i64 %c31
  %c33 = load i8, ptr %c32, align 1
  %c34 = sext i8 %c33 to i32
  %c35 = icmp sgt i32 %c34, 57
  br i1 %c35, label %c36, label %c49

c36:                                               ; preds = %c28, %c20
  %c37 = load ptr, ptr %c3, align 8
  %c38 = load i32, ptr %c4, align 4
  %c39 = sext i32 %c38 to i64
  %c40 = getelementptr i8, ptr %c37, i64 %c39
  %c41 = load i8, ptr %c40, align 1
  %c42 = sext i8 %c41 to i32
  %c43 = icmp ne i32 %c42, 45
  br i1 %c43, label %c44, label %c49

c44:                                               ; preds = %c36
  %c45 = load i32, ptr %c5, align 4
  %c46 = icmp ne i32 %c45, 0
  br i1 %c46, label %c47, label %c48

c47:                                               ; preds = %c44
  br label %c79

c48:                                               ; preds = %c44
  br label %c16

c49:                                               ; preds = %c36, %c28
  %c50 = load ptr, ptr %c3, align 8
  %c51 = load i32, ptr %c4, align 4
  %c52 = sext i32 %c51 to i64
  %c53 = getelementptr i8, ptr %c50, i64 %c52
  %c54 = load i8, ptr %c53, align 1
  %c55 = sext i8 %c54 to i32
  %c56 = icmp eq i32 %c55, 45
  br i1 %c56, label %c57, label %c64

c57:                                               ; preds = %c49
  %c58 = load i32, ptr %c5, align 4
  %c59 = icmp ne i32 %c58, 0
  br i1 %c59, label %c60, label %c63

c60:                                               ; preds = %c57
  %c61 = load i32, ptr %c5, align 4
  %c62 = sub nsw i32 0, %c61
  store i32 %c62, ptr %c5, align 4
  br label %c79

c63:                                               ; preds = %c57
  br label %c78

c64:                                               ; preds = %c49
  %c65 = load ptr, ptr %c3, align 8
  %c66 = load i32, ptr %c4, align 4
  %c67 = sext i32 %c66 to i64
  %c68 = getelementptr i8, ptr %c65, i64 %c67
  %c69 = load i8, ptr %c68, align 1
  %c70 = sext i8 %c69 to i32
  %c71 = sub nsw i32 %c70, 48
  %c72 = load i32, ptr %c6, align 4
  %c73 = mul nsw i32 %c71, %c72
  %c74 = load i32, ptr %c5, align 4
  %c75 = add nsw i32 %c74, %c73
  store i32 %c75, ptr %c5, align 4
  %c76 = load i32, ptr %c6, align 4
  %c77 = mul nsw i32 %c76, 10
  store i32 %c77, ptr %c6, align 4
  br label %c78

c78:                                               ; preds = %c64, %c63
  br label %c16

c79:                                               ; preds = %c60, %c47, %c16
  %c80 = load i32, ptr %c5, align 4
  ret i32 %c80
}

; Function Attrs: noinline nounwind optnone uwtable
define i32 @mystrlen(ptr %s0) {
  %s2 = alloca ptr, align 8
  %s3 = alloca i32, align 4
  store ptr %s0, ptr %s2, align 8
  store i32 0, ptr %s3, align 4
  br label %s4

s4:                                                ; preds = %s12, %1
  %s5 = load ptr, ptr %s2, align 8
  %s6 = load i32, ptr %s3, align 4
  %s7 = add nsw i32 %s6, 1
  store i32 %s7, ptr %s3, align 4
  %s8 = sext i32 %s6 to i64
  %s9 = getelementptr i8, ptr %s5, i64 %s8
  %s10 = load i8, ptr %s9, align 1
  %s11 = icmp ne i8 %s10, 0
  br i1 %s11, label %s12, label %s13

s12:                                               ; preds = %4
  br label %s4

s13:                                               ; preds = %4
  %s14 = load i32, ptr %s3, align 4
  ret i32 %s14
}

; Function Attrs: noinline nounwind optnone uwtable
define i32 @main(i32  %a0, ptr  %a1){
  %a3 = alloca i32, align 4
  %a4 = alloca i32, align 4
  %a5 = alloca ptr, align 8
  %a6 = alloca i32, align 4
  %a7 = alloca i32, align 4
  %a8 = alloca ptr, align 8
  %a9 = alloca i32, align 4
  store i32 0, ptr %a3, align 4
  store i32 %a0, ptr %a4, align 4
  store ptr %a1, ptr %a5, align 8
  store i32 0, ptr %a6, align 4
  store i32 0, ptr %a7, align 4
  br label %a10

a10:                                               ; preds = %a27, %2
  %a11 = load i32, ptr %a7, align 4
  %a12 = load i32, ptr %a4, align 4
  %a13 = icmp slt i32 %a11, %a12
  br i1 %a13, label %a14, label %a30

a14:                                               ; preds = %a10
  %a15 = load ptr, ptr %a5, align 8
  %a16 = load i32, ptr %a7, align 4
  %a17 = sext i32 %a16 to i64
  %a18 = getelementptr  ptr, ptr %a15, i64 %a17
  %a19 = load ptr, ptr %a18, align 8
  store ptr %a19, ptr %a8, align 8
  %a20 = load ptr, ptr %a8, align 8
  %a21 = call i32 @mystrlen(ptr  %a20)
  store i32 %a21, ptr %a9, align 4
  %a22 = load ptr, ptr %a8, align 8
  %a23 = load i32, ptr %a9, align 4
  %a24 = call i32 @char2int(ptr  %a22, i32  %a23)
  %a25 = load i32, ptr %a6, align 4
  %a26 = add nsw i32 %a25, %a24
  store i32 %a26, ptr %a6, align 4
  br label %a27

a27:                                               ; preds = %a14
  %a28 = load i32, ptr %a7, align 4
  %a29 = add nsw i32 %a28, 1
  store i32 %a29, ptr %a7, align 4
  br label %a10

a30:                                               ; preds = %a10
  %a31 = load i32, ptr %a6, align 4
  ;! %print_res = call i32 (ptr, ...) @printf(ptr @.str, i32 %a31)
  ;! ret i32 0
  ret i32 %a31
}
