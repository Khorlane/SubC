bits 32

align 4
FortyTwo: dd 0

TestIt:
  mov eax, 42
  mov dword [FortyTwo], eax
  ret
