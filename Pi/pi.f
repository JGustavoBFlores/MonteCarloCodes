      PROGRAM pi  
      IMPLICIT REAL*8 (A-H,O-Z) 
      DIMENSION HOLD(1000)
      
      CALL SUB1(A,10.0D0,20.0D0)
      PRINT*, A
      STop
      n=10**9
      aHOLD=0.0D0
      HOLD=0.0D0
      DO K=1,100
      J=0
      i=0
      DO WHILE(i.lt.n)
      i=i+1
      call random_number(x)
      call random_number(y)
      if((x**2 +y**2).LE.1.0D0)then
       J=J+1
      END IF
      END DO
      HOLD(K)=4.0d0*J/n
      PRINT*, HOLD(K)
      aHOLD=ahold+HOLD(K)
      END DO
      print*, ahold/(K-1)
 
      END PROGRAM

      SUBROUTINE SUB1(A,B,C)
      IMPLICIT REAL*8 (A-H,O-Z) 
      A=B+C
      END SUBROUTINE



