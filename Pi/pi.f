      PROGRAM montePi  
      IMPLICIT REAL*8 (A-H,O-Z) 
      PARAMETER (NDH=10**9,ITER=5)

      Pi=0.D0
      DO K=1,ITER
       I=0
       J=0
       DO WHILE(I.LT.NDH)
        I=I+1
        CALL RANDOM_NUMBER(X)
        CALL RANDOM_NUMBER(Y)
        IF((X**2 + Y**2).LT.1.0D0)then
         J=J+1
        END IF
       END DO
       Pi=(Pi+4.0D0*J/I)
C      print*, 4.0d0*J/I
      END DO

      PRINT*, 'Pi is, approximately, equal to',Pi/(K-1)
      PRINT 102, 'This took',K-1,' iterations with',NDH,' shots.'
 102  FORMAT(A,I2,A,I11,A)
 
      END PROGRAM
