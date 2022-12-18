
Module modParam


! PARAMETER (MX=105,NP=100,NQ=101)
Integer :: MX, NP, NQ

Parameter(MX = 105, NP = 100, NQ = 101)


! COMMON /FILEOUT/
Integer :: IRAO, IDIF, IRAD

! COMMON /PAI/
Double Precision :: PI, PI05, PI2

! COMMON /MDT/
Double Precision :: CMAS, C22, OG, KZZ, GM

! COMMON /ELM/
Double Precision :: XP(MX), YP(MX), XQ(NQ), YQ(NQ)

! COMMON /VN2/
Double Precision :: VN(3,NP)

! COMMON /FAI/
Complex(kind = 8) :: ZFI(4,NP)

! COMMON /FCE/
Complex(kind = 8) :: ZAB(3,3), ZEXF(3)

! COMMON /MTN/
Complex(kind = 8) :: ZMTNO(3)


End Module