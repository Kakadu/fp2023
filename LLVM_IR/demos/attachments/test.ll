@dd = global i32 0, align 4
@bb = global i32 32, align 4

; Function Attrs: noinline nounwind optnone uwtable
define i32 @ds() {
  %1 = alloca i32, align 4
  store i32 5, ptr %1, align 4
  %2 = load i32, ptr %1, align 4
  %3 = load i32, ptr @bb, align 4
  %4 = add nsw i32 %3, %2
  store i32 %4, ptr @bb, align 4
  %5 = load i32, ptr @dd, align 4
  store i32 %5, ptr %1, align 4
  %6 = load i32, ptr %1, align 4
  ret i32 %6
}

; Function Attrs: noinline nounwind optnone uwtable
define i32 @main(){
  %1 = call i32 @ds()
  %2 = call i32 @ds()
  ret i32 0
}
