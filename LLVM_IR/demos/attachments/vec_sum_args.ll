
;! @.str = global [11 x i8] c"%d %d %d \0A\00", align 1
;! declare i32 @printf(ptr , ...)


; Function Attrsa: noinline nounwind optnone uwtable
define  i32 @char2int(ptr  %a0, i32  %a1) {
  %a3 = alloca ptr, align 8
  %a4 = alloca i32, align 4
  %a5 = alloca i32, align 4
  %a6 = alloca i32, align 4
  store ptr %a0, ptr %a3, align 8
  store i32 %a1, ptr %a4, align 4
  store i32 0, ptr %a5, align 4
  store i32 1, ptr %a6, align 4
  %a7 = load i32, ptr %a4, align 4
  %a8 = icmp slt i32 %a7, 0
  br i1 %a8, label %a9, label %a12

a9:                                                ; preds = %a2
  %a10 = load i32, ptr %a4, align 4
  %a11 = sub nsw i32 0, %a10
  br label %a14

a12:                                               ; preds = %a2
  %a13 = load i32, ptr %a4, align 4
  br label %a14

a14:                                               ; preds = %a12, %a9
  %a15 = phi i32 [ %a11, %a9 ], [ %a13, %a12 ]
  store i32 %a15, ptr %a4, align 4
  br label %a16

a16:                                               ; preds = %a78, %a48, %a14
  %a17 = load i32, ptr %a4, align 4
  %a18 = add nsw i32 %a17, -1
  store i32 %a18, ptr %a4, align 4
  %a19 = icmp ne i32 %a17, 0
  br i1 %a19, label %a20, label %a79

a20:                                               ; preds = %a16
  %a21 = load ptr, ptr %a3, align 8
  %a22 = load i32, ptr %a4, align 4
  %a23 = sext i32 %a22 to i64
  %a24 = getelementptr  i8, ptr %a21, i64 %a23
  %a25 = load i8, ptr %a24, align 1
  %a26 = sext i8 %a25 to i32
  %a27 = icmp slt i32 %a26, 48
  br i1 %a27, label %a36, label %a28

a28:                                               ; preds = %a20
  %a29 = load ptr, ptr %a3, align 8
  %a30 = load i32, ptr %a4, align 4
  %a31 = sext i32 %a30 to i64
  %a32 = getelementptr  i8, ptr %a29, i64 %a31
  %a33 = load i8, ptr %a32, align 1
  %a34 = sext i8 %a33 to i32
  %a35 = icmp sgt i32 %a34, 57
  br i1 %a35, label %a36, label %a49

a36:                                               ; preds = %a28, %a20
  %a37 = load ptr, ptr %a3, align 8
  %a38 = load i32, ptr %a4, align 4
  %a39 = sext i32 %a38 to i64
  %a40 = getelementptr  i8, ptr %a37, i64 %a39
  %a41 = load i8, ptr %a40, align 1
  %a42 = sext i8 %a41 to i32
  %a43 = icmp ne i32 %a42, 45
  br i1 %a43, label %a44, label %a49

a44:                                               ; preds = %a36
  %a45 = load i32, ptr %a5, align 4
  %a46 = icmp ne i32 %a45, 0
  br i1 %a46, label %a47, label %a48

a47:                                               ; preds = %a44
  br label %a79

a48:                                               ; preds = %a44
  br label %a16

a49:                                               ; preds = %a36, %a28
  %a50 = load ptr, ptr %a3, align 8
  %a51 = load i32, ptr %a4, align 4
  %a52 = sext i32 %a51 to i64
  %a53 = getelementptr  i8, ptr %a50, i64 %a52
  %a54 = load i8, ptr %a53, align 1
  %a55 = sext i8 %a54 to i32
  %a56 = icmp eq i32 %a55, 45
  br i1 %a56, label %a57, label %a64

a57:                                               ; preds = %a49
  %a58 = load i32, ptr %a5, align 4
  %a59 = icmp ne i32 %a58, 0
  br i1 %a59, label %a60, label %a63

a60:                                               ; preds = %a57
  %a61 = load i32, ptr %a5, align 4
  %a62 = sub nsw i32 0, %a61
  store i32 %a62, ptr %a5, align 4
  br label %a79

a63:                                               ; preds = %a57
  br label %a78

a64:                                               ; preds = %a49
  %a65 = load ptr, ptr %a3, align 8
  %a66 = load i32, ptr %a4, align 4
  %a67 = sext i32 %a66 to i64
  %a68 = getelementptr  i8, ptr %a65, i64 %a67
  %a69 = load i8, ptr %a68, align 1
  %a70 = sext i8 %a69 to i32
  %a71 = sub nsw i32 %a70, 48
  %a72 = load i32, ptr %a6, align 4
  %a73 = mul nsw i32 %a71, %a72
  %a74 = load i32, ptr %a5, align 4
  %a75 = add nsw i32 %a74, %a73
  store i32 %a75, ptr %a5, align 4
  %a76 = load i32, ptr %a6, align 4
  %a77 = mul nsw i32 %a76, 10
  store i32 %a77, ptr %a6, align 4
  br label %a78

a78:                                               ; preds = %a64, %a63
  br label %a16

a79:                                               ; preds = %a60, %a47, %a16
  %a80 = load i32, ptr %a5, align 4
  ret i32 %a80
}

; Function Attrsa: noinline nounwind optnone uwtable
define  i32 @mystrlen(ptr  %a0) {
  %a2 = alloca ptr, align 8
  %a3 = alloca i32, align 4
  store ptr %a0, ptr %a2, align 8
  store i32 0, ptr %a3, align 4
  br label %a4

a4:                                                ; preds = %a12, %a1
  %a5 = load ptr, ptr %a2, align 8
  %a6 = load i32, ptr %a3, align 4
  %a7 = add nsw i32 %a6, 1
  store i32 %a7, ptr %a3, align 4
  %a8 = sext i32 %a6 to i64
  %a9 = getelementptr  i8, ptr %a5, i64 %a8
  %a10 = load i8, ptr %a9, align 1
  %a11 = icmp ne i8 %a10, 0
  br i1 %a11, label %a12, label %a13

a12:                                               ; preds = %a4
  br label %a4

a13:                                               ; preds = %a4
  %a14 = load i32, ptr %a3, align 4
  ret i32 %a14
}

; Function Attrsa: noinline nounwind optnone uwtable
define   <3 x i32> @main(i32  %a0, ptr  %a1) {
a2:
  %a3 = alloca i32, align 4
  %a4 = alloca ptr, align 8
  %a5 = alloca i32, align 4
  %a6 = alloca i32, align 4
  %a8 = alloca <3 x i32>, align 4
  %a9 = alloca i32, align 4
  %a10 = alloca ptr, align 8
  %a11 = alloca i32, align 4
  store i32 %a0, ptr %a3, align 4
  store ptr %a1, ptr %a4, align 8
  store i32 0, ptr %a5, align 4
  store i32 0, ptr %a6, align 4
  %vector_a1 = insertelement <3 x i32> <i32 1, i32 1, i32 1>, i32 0, i32 0 
  %vector_a2 = insertelement <3 x i32> %vector_a1, i32 0, i32 1
  %vector_a3 = insertelement <3 x i32> %vector_a2, i32 0, i32 2 
  %a15 = load i32, ptr %a3, align 4
  %a16 = sub nsw i32 %a15, 1
  %a17 = srem i32 %a16, 3
  %a18 = icmp eq i32 %a17, 0
  br i1 %a18, label %a19, label %a71

a19:                                               ; preds = %a2
  br label %a20
  ; while
a20:                                               ; preds = %a67, %a19
  %vec_a_iter = phi <3 x i32> [%vector_a3, %a19], [%vec_a_new, %a67]
  %a21 = load i32, ptr %a6, align 4
  %a22 = mul nsw i32 %a21, 3
  %a23 = add nsw i32 %a22, 1
  %a24 = load i32, ptr %a3, align 4
  %a25 = icmp slt i32 %a23, %a24
  br i1 %a25, label %a26, label %a70

a26:                                               ; preds = %a20
  store i32 0, ptr %a9, align 4
  br label %a27

a27:                                               ; preds = %a47, %a26
  %vector_b = phi <3 x i32> [<i32 0, i32 0, i32 0>, %a26], [%vector_b_new, %a47]
  %a28 = load i32, ptr %a9, align 4
  %a29 = icmp slt i32 %a28, 3
  br i1 %a29, label %a30, label %a50

a30:                                               ; preds = %a27
  %a31 = load ptr, ptr %a4, align 8
  %a32 = load i32, ptr %a6, align 4
  %a33 = mul nsw i32 %a32, 3
  %a34 = add nsw i32 1, %a33
  %a35 = load i32, ptr %a9, align 4
  %a36 = add nsw i32 %a34, %a35
  %a37 = sext i32 %a36 to i64
  %a38 = getelementptr  ptr, ptr %a31, i64 %a37
  %a39 = load ptr, ptr %a38, align 8
  store ptr %a39, ptr %a10, align 8
  %a40 = load ptr, ptr %a10, align 8
  %a41 = load ptr, ptr %a10, align 8
  %a42 = call i32 @mystrlen(ptr  %a41)
  %a43 = call i32 @char2int(ptr  %a40, i32  %a42)
  %a44 = load i32, ptr %a9, align 4
  %a45 = sext i32 %a44 to i64
  %vector_b_new = insertelement <3 x i32> %vector_b, i32 %a43, i64 %a45
  br label %a47

a47:                                               ; preds = %a30
  %a48 = load i32, ptr %a9, align 4
  %a49 = add nsw i32 %a48, 1
  store i32 %a49, ptr %a9, align 4
  br label %a27

a50:                                               ; preds = %a27
  store i32 0, ptr %a11, align 4
  br label %a51

a51:                                               ; preds = %a64, %a50
  %vec_a_new = add nsw <3 x i32> %vector_b, %vec_a_iter
  br label %a67

a67:                                               ; preds = %a51
  %a68 = load i32, ptr %a6, align 4
  %a69 = add nsw i32 %a68, 1
  store i32 %a69, ptr %a6, align 4
  br label %a20

a70:                                               ; preds = %a20
  br label %a71

a71:                                               ; preds = %a70, %a2
  %vector_a_fin = phi <3 x i32> [%vector_a3, %a2], [%vec_a_iter, %a70]
  ;! %va0 = extractelement  <3 x i32> %vector_a_fin, i64 0
  ;! %va1 = extractelement  <3 x i32> %vector_a_fin, i64 1
  ;! %va2 = extractelement  <3 x i32> %vector_a_fin, i64 2
  ;! %a78 = call i32 (ptr, ...) @printf(ptr  @.str, i32  %va0, i32  %va1, i32  %va2)
  ret <3 x i32> %vector_a_fin
}
