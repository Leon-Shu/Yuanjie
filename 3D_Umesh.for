C     USER INPUT FOR ADAPTIVE MESH CONSTRAINT
C
      SUBROUTINE UMESHMOTION(UREF,ULOCAL,NODE,NNDOF,
     $     LNODETYPE,ALOCAL,NDIM,TIME,DTIME,PNEWDT,
     $     KSTEP,KINC,KMESHSWEEP,JMATYP,JGVBLOCK,LSMOOTH)
C
      INCLUDE 'ABA_PARAM.INC'
C
      DIMENSION ULOCAL(NDIM)
      DIMENSION ALOCAL(NDIM,*),TIME(2)
      DIMENSION JMATYP(*),JGVBLOCK(*)
C
C     USER DEFINED DIMENSION STATEMENTS
C
      CHARACTER*80 PARTNAME
C     The dimensions of the variables ARRAY
C     must be set equal to or greater than 15
      DIMENSION ARRAY(3000)      
      PARAMETER (NELEMMAX=15) 
      DIMENSION JELEMLIST(NELEMMAX),JELEMTYPE(NELEMMAX)           
      PARAMETER (TOLER=1.0D-6)
      PARAMETER (DELTAHCRIT=0.001)
           
      COMMON/WEAR/
     $ SACCUH(3000),
     $ SNODES(3000),
     $ SLOCNODES(3000),
     $ SOLDSLIP(3000),
     $ SNEWSLIP(3000),
     $ SINCSLIP(3000),
     $ SPRESS(3000),
     $ SSHEAR(3000),
     $ SXCOORD(3000),
     $ SYCOORD(3000),
     $ SZCOORD(3000),
     $ SLAVEREG(100000),
     $ ITOTALNUM,
     $ ISCLOCK    
     
C     debug flag      
      DEBUG=1
      
C     WEAR PARAMETERS
      DELTAN=1
      PK=2E-8 
      
      OPEN(unit=16,file=
     $ 'E:\temp\Out-UMESHMOTION.txt',
     $ status='unknown')
      OPEN(unit=17,file=
     $ 'E:\temp\Out-Slave.txt',
     $ status='unknown')
      OPEN(unit=18,file=
     $ 'E:\temp\Out-Coord.txt',
     $ status='unknown')
      OPEN(unit=19,file=
     $ 'E:\temp\Out-Surfv.txt',
     $ status='unknown')          
            
      LOCNUM = 0
      JRCD1 = 0
      JRCD2 = 0
      JRCD3 = 0
      JRCD4 = 0
      JRCD5 = 0
      JTYP = 0
      PARTNAME = ' '            
      NELEMS = NELEMMAX
      
      CALL GETPARTINFO(NODE,0,PARTNAME,LOCNUM,JRCD1)
      CALL GETNODETOELEMCONN(NODE,NELEMS,JELEMLIST,JELEMTYPE,
     $     JRCD2,JGVBLOCK)
      CALL GETVRMAVGATNODE(NODE,JTYP,'CSTRESS',ARRAY,JRCD3,
     $     JELEMLIST,NELEMS,JMATYP,JGVBLOCK)           
      CPRESS = ARRAY(1)
      CSHEAR = SQRT(ARRAY(2)**2+ARRAY(3)**2)
      CALL GETVRMAVGATNODE(NODE,JTYP,'CDISP',ARRAY,JRCD4,
     $     JELEMLIST,NELEMS,JMATYP,JGVBLOCK)
      COPEN = ARRAY(1)
      CSLIP = SQRT(ARRAY(2)**2+ARRAY(3)**2)          
      CALL GETVRN(NODE,'COORD',ARRAY,JRCD5,JGVBLOCK,LTRN) 
      XCOORD=ARRAY(1) 
      YCOORD=ARRAY(2)
      ZCOORD=ARRAY(3)
      
C     PRINT SOME DEBUG INFORMATION      
      IF ((DEBUG.NE.0).AND.(KMESHSWEEP.EQ.0)) THEN
C      IF (DEBUG.NE.0) THEN
          WRITE(16,*) '----------------------------------------------------    
     $------------------------------'
          WRITE(16,*) 'UMESHMOTION'
          WRITE(16,*) 'KSTEP ',KSTEP
          WRITE(16,*) 'KINC ',KINC
          WRITE(16,*) 'KMESHSWEEP ',KMESHSWEEP
          WRITE(16,*) 'UREF ',UREF 
          WRITE(16,*) ''                   
          WRITE(16,*) 'JRCD1 ',JRCD1
          WRITE(16,*) 'NODE ',NODE
          WRITE(16,*) 'LOCNUM ',LOCNUM
          WRITE(16,*) 'JRCD2 ',JRCD2          
          WRITE(16,*) 'JELEMLIST WITH ',NELEMS,' ELEMENTS'
          DO K1 = 1,NELEMS
            IF (JELEMTYPE(K1).EQ.1) THEN
                CALL GETPARTINFO(JELEMLIST(K1),1,PARTNAME,LOCENUM,JRCD)
                WRITE(16,*) K1,JELEMLIST(K1),LOCENUM,JELEMTYPE(K1),JRCD
            ELSE 
                WRITE(16,*) K1,JELEMLIST(K1),'  Internal ',JELEMTYPE(K1)
            END IF
C              WRITE(16,*) K1,JELEMLIST(K1),JELEMTYPE(K1)
          END DO
          WRITE(16,*) 'JRCD3 ',JRCD3
          WRITE(16,*) 'CPRESS ',CPRESS
          WRITE(16,*) 'CSHEAR ',CSHEAR
          WRITE(16,*) 'JRCD4 ',JRCD4
          WRITE(16,*) 'COPEN ',COPEN
          WRITE(16,*) 'CSLIP ',CSLIP
          WRITE(16,*) 'JRCD5 ',JRCD5
          WRITE(16,*) 'XCOORD ',XCOORD
          WRITE(16,*) 'YCOORD ',YCOORD          
      END IF
      
      IF ((NODE==SNODES(1)).AND.(KMESHSWEEP.NE.0)) THEN
          ITOTALNUM=ISCLOCK
          DO K1=1,ITOTALNUM
              SOLDSLIP(K1)=SNEWSLIP(K1)
          END DO
      END IF 

      IF (SLAVEREG(NODE)==0) THEN       
          ISCLOCK=ISCLOCK+1
          SLAVEREG(NODE)=ISCLOCK
          SNODES(ISCLOCK)=NODE
          SLOCNODES(ISCLOCK)=LOCNUM
      END IF
      
      SXCOORD(SLAVEREG(NODE))=XCOORD
      SYCOORD(SLAVEREG(NODE))=YCOORD
      SZCOORD(SLAVEREG(NODE))=ZCOORD
      SNEWSLIP(SLAVEREG(NODE))=CSLIP
      SPRESS(SLAVEREG(NODE))=CPRESS
      SSHEAR(SLAVEREG(NODE))=CSHEAR
      DELTACSLIP=CSLIP-SOLDSLIP(SLAVEREG(NODE))
      DELTAH=ABS(PK*CSHEAR*DELTACSLIP/DTIME)
      SACCUH(SLAVEREG(NODE))=SACCUH(SLAVEREG(NODE))+DELTAH*DTIME
      
C     PRINT SOME DEBUG INFORMATION
      IF ((DEBUG.NE.0).AND.(KMESHSWEEP.EQ.0)) THEN
C      IF (DEBUG.NE.0) THEN
          WRITE(17,*) '----------------------------------------------------    
     $------------------------------'
          WRITE(17,*) KINC,KMESHSWEEP,NODE,LOCNUM,CPRESS,CSHEAR,
     $            SOLDSLIP(SLAVEREG(NODE)),SNEWSLIP(SLAVEREG(NODE))
      END IF
      
      IF ((KSTEP.EQ.3).AND.(KINC==1).AND.(KMESHSWEEP.EQ.0))THEN 
          WRITE(18,*) LOCNUM,XCOORD,YCOORD,ZCOORD  
      END IF
            
 500  IF (KSTEP.GE.3) THEN
          ULOCAL(3)=ULOCAL(3)-DELTAH
          
          IF ((DELTAH*DTIME).GT.DELTAHCRIT) THEN 
          PNEWDT_0=PNEWDT 
          PNEWDT_1=0.99*DELTAHCRIT/DELTAH 
          IF(PNEWDT_1.LT.PNEWDT_0)THEN 
              PNEWDT=PNEWDT_1  
              WRITE (6,*) 'CHANGING TIME INCREMENT FROM ',PNEWDT_0 
              WRITE (6,*) 'TO ',PNEWDT 
              WRITE (6,*) 'BASED ON NODE ',NODE 
          END IF 
          END IF
          
          IF (KMESHSWEEP.EQ.0) THEN
              WRITE(19,1000),LOCNUM,XCOORD,YCOORD,DELTAH,
     $                       SACCUH(SLAVEREG(NODE))
          END IF 
      END IF
 1000 FORMAT(i5,1x,e13.6,1x,e13.6,1x,e13.6,1x,e13.6,1x,e13.6,1x,e13.6,
     $1x,e13.6)  
      RETURN
      END
