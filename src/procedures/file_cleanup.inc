!> Replace every literal `NaN` token in a text file with `0.0`.
!! @param[in] FNAME Path of the file to rewrite in place.
!! @warning Uses units 1 and 2 plus the fixed temporary name `temp_fix.dxf`;
!!          it is not reentrant and truncates input records after 204 characters.
      SUBROUTINE NONAN(FNAME)
C     Direct sed replacement: s/NaN/0.0/g
      
      CHARACTER*(*) FNAME
      CHARACTER*204 LINE
      CHARACTER*204 OUTLINE
      INTEGER I, J, L, N
      LOGICAL DONE
      
C     Process to temporary file
      OPEN(1, FILE=FNAME)
      OPEN(2, FILE='temp_fix.dxf')
      
      DONE = .FALSE.
      DO WHILE (.NOT. DONE)
          READ(1, '(A)', IOSTAT=I) LINE
          IF (I .LT. 0) THEN
              DONE = .TRUE.
          ELSE
C             Get actual line length
              L = 0
              DO N = 1, 204
                  IF (LINE(N:N) .NE. ' ') THEN
                      L = N
                  ENDIF
              END DO
              
C             Initialize output line
              OUTLINE = ' '
              J = 1
              I = 1
              
C             Process character by character
              DO WHILE (I .LE. L-2)
                  IF (LINE(I:I+2) .EQ. 'NaN') THEN
C                     Copy characters before NaN
                      IF (J .LT. I) THEN
                          OUTLINE(J:I-1) = LINE(J:I-1)
                          J = I
                      ENDIF
C                     Add replacement
                      OUTLINE(J:J+2) = '0.0'
                      J = J + 3
                      I = I + 3
                  ELSE
                      OUTLINE(J:J) = LINE(I:I)
                      J = J + 1
                      I = I + 1
                  ENDIF
              END DO
              
C             Copy remaining characters
              DO WHILE (I .LE. L)
                  OUTLINE(J:J) = LINE(I:I)
                  J = J + 1
                  I = I + 1
              END DO
              
C             Write the line
              WRITE(2, '(A)') OUTLINE(1:J-1)
          ENDIF
      END DO
      
      CLOSE(1)
      CLOSE(2)
      
C     Copy back to original
      OPEN(1, FILE='temp_fix.dxf')
      OPEN(2, FILE=FNAME, STATUS='REPLACE')
      
      DONE = .FALSE.
      DO WHILE (.NOT. DONE)
          READ(1, '(A)', IOSTAT=I) LINE
          IF (I .LT. 0) THEN
              DONE = .TRUE.
          ELSE
              WRITE(2, '(A)') TRIM(LINE)
          ENDIF
      END DO
      
      CLOSE(1)
      CLOSE(2)
      
C     Clean up
      OPEN(1, FILE='temp_fix.dxf')
      CLOSE(1, STATUS='DELETE')
      
      return
      END


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


!> Replace DXF NaN tokens and ensure that the document has an EOF marker.
!! @param[in] FILENAME Path of the DXF file to rewrite in place.
!! @warning Uses units 10 and 11 and the fixed temporary name `dxf_fixed.tmp`;
!!          it must not run concurrently in the same working directory.
      SUBROUTINE FIX_DXF_NAN(FILENAME)
C     Complete fix for AutoCAD DXF with NaN replacement
      
      CHARACTER*(*) FILENAME
      CHARACTER*2048 LINE, TEMPFILE
      INTEGER I, J, L, IOS
      LOGICAL HAS_EOF, PROCESSING
      
      TEMPFILE = 'dxf_fixed.tmp'
      
C     First pass: replace NaN and check structure
      OPEN(10, FILE=FILENAME, STATUS='OLD')
      OPEN(11, FILE=TEMPFILE, STATUS='NEW')
      
      HAS_EOF = .FALSE.
      PROCESSING = .TRUE.
      
      DO WHILE (PROCESSING)
          READ(10, '(A)', IOSTAT=IOS) LINE
          
          IF (IOS .LT. 0) THEN
              PROCESSING = .FALSE.
          ELSE
C             Check if this is EOF marker
              L = LEN_TRIM(LINE)
              IF (L .GE. 3) THEN
                  IF (LINE(1:3) .EQ. '  0') THEN
                      IF (INDEX(LINE, 'EOF') .GT. 0) THEN
                          HAS_EOF = .TRUE.
                      ENDIF
                  ENDIF
              ENDIF
              
C             Replace NaN in this line
              CALL REPLACE_IN_STRING(LINE, 'NaN', '0.0')
              
C             Write processed line
              WRITE(11, '(A)') TRIM(LINE)
          ENDIF
      END DO
      
      CLOSE(10)
      CLOSE(11)
      
C     Second pass: ensure proper EOF and copy back
      OPEN(10, FILE=TEMPFILE, STATUS='OLD')
      OPEN(11, FILE=FILENAME, STATUS='REPLACE')
      
      PROCESSING = .TRUE.
      DO WHILE (PROCESSING)
          READ(10, '(A)', IOSTAT=IOS) LINE
          
          IF (IOS .LT. 0) THEN
              PROCESSING = .FALSE.
              
C             Add EOF if missing
              IF (.NOT. HAS_EOF) THEN
                  WRITE(11, '(A)') '  0'
                  WRITE(11, '(A)') 'EOF'
              ENDIF
          ELSE
              WRITE(11, '(A)') TRIM(LINE)
          ENDIF
      END DO
      
      CLOSE(10)
      CLOSE(11)
      
C     Clean up
      OPEN(10, FILE=TEMPFILE)
      CLOSE(10, STATUS='DELETE')
      
      END
      
!> Replace all non-overlapping occurrences of one token in a fixed-length string.
!! @param[in,out] STR String to scan and overwrite with the replacement result.
!! @param[in] OLD Nonempty token to locate; trailing blanks are ignored.
!! @param[in] NEW Replacement token; trailing blanks are ignored.
!! @warning The intermediate result is limited to 2,048 characters.
      SUBROUTINE REPLACE_IN_STRING(STR, OLD, NEW)
C     Replace all occurrences of OLD with NEW in STR
      
      CHARACTER*(*) STR, OLD, NEW
      CHARACTER*2048 RESULT
      INTEGER I, J, L, OLDLEN, NEWLEN, POS
      
      OLDLEN = LEN_TRIM(OLD)
      NEWLEN = LEN_TRIM(NEW)
      L = LEN_TRIM(STR)
      RESULT = ' '
      J = 1
      I = 1
      
      DO WHILE (I .LE. L)
          IF (I .LE. L-OLDLEN+1) THEN
              IF (STR(I:I+OLDLEN-1) .EQ. OLD(1:OLDLEN)) THEN
                  IF (J .LT. I) THEN
                      RESULT(J:I-1) = STR(J:I-1)
                      J = I
                  ENDIF
                  RESULT(J:J+NEWLEN-1) = NEW(1:NEWLEN)
                  J = J + NEWLEN
                  I = I + OLDLEN
              ELSE
                  RESULT(J:J) = STR(I:I)
                  J = J + 1
                  I = I + 1
              ENDIF
          ELSE
              RESULT(J:J) = STR(I:I)
              J = J + 1
              I = I + 1
          ENDIF
      END DO
      
      STR = RESULT(1:J-1)
      
      END


c     DXF not working!!! :-(  ^^^^^^







ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     SUBROUTINE loc2glo2D(n,xloc,yloc,xglo,yglo,x0,y0,angle)
c     Local to global coordinates 2D by translation and rotation
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

