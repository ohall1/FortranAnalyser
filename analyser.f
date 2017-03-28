      PROGRAM GREAT

C     Read and analyse GREAT format data - version 3.3.0
C
C     See:
C     http://npg.dl.ac.uk/documents/edoc504/edoc504.html
C     http://npg.dl.ac.uk/DataAcq/TSformat.html

      IMPLICIT NONE                                 
C
C     External functions
C
      EXTERNAL timestamp
C
      DOUBLE PRECISION dfloat
C
      INTEGER and
      INTEGER*8 iargc
      INTEGER rshift
      INTEGER*8 timestamp
C
      REAL secnds
C
C     Parameter variables
C
      INTEGER block_length
      PARAMETER (block_length = 16384)
C
C     Local variables
C
      CHARACTER*255 arg
      CHARACTER*(block_length*4) buffer
      CHARACTER*255 filename
C
      INTEGER adc_data, block_number, block_ptr, ch, channel
      INTEGER channel_ident, data_block(block_length)
      INTEGER*8 dead(32)
      INTEGER event, fail
      INTEGER*8 first_ts
      INTEGER i
      INTEGER*2 i2(2), i2_2(4)
      INTEGER i4, i4_2(2)
      INTEGER*8 i8, idle(32)
      INTEGER information, information_index, io_error, items(0:32,0:31)
      INTEGER j, j4
      INTEGER*8 last_ts
      INTEGER mbs_data, module
      INTEGER*8 pause(32)
      INTEGER range
      INTEGER*8 resume(32)
      INTEGER sample_length, sum(0:31)
      INTEGER*8 sync(32)
      INTEGER tag, tag_old
      INTEGER*8 ts
      INTEGER ts28, ts48
      INTEGER*8 ts_old
      INTEGER type, type_old
C
      LOGICAL verbose
C
      REAL dt, t1, t2

      EQUIVALENCE ( data_block, buffer )
      EQUIVALENCE ( i4, i2( 1 ) )
      EQUIVALENCE ( i8, i4_2(1) )

      SAVE data_block, block_ptr, block_number, event

C----67---------------------------------------------------------------72------80
C     Announce program

      WRITE( 6, 9000 )

C     Initialise variables

      block_ptr = 1
      block_number = 0
      event = 0

      i4 = 0
      i8 = 0

      ts_old = 0
      tag_old = 0
      type_old = 0
      first_ts = 0
      last_ts = 0
      ts28 = 0
      ts48 = 0

      type = 0
      
      verbose = .FALSE.
      
      DO i = 0, 32
       DO j = 0, 31
        sum( j ) = 0
        items( i, j ) = 0
       ENDDO
      ENDDO

      DO i = 1, 32
       pause( i ) = -1
       resume( i ) = -1
       dead( i ) = 0
       sync( i ) = 0
       idle( i ) = 0
      ENDDO

C     Get input filename from command line argument

      DO i = 1, iargc()
       CALL getarg( i, arg )
       IF ( arg.EQ.'v') THEN
        verbose = .TRUE.
        write(6,*) ' verbose'
       ELSE
        filename = arg
       ENDIF
      ENDDO
      IF ( iargc().LT.1 .OR. iargc().GT.2 ) THEN
       WRITE( 6, * ) ' Usage: ./a.exe [v] <filename>'
       STOP
      ENDIF

      t1 = SECNDS( 0.0 )

C     Open file

      OPEN( 10, RECL = 4 * block_length,
     +      FILE = filename, STATUS = 'OLD', ACCESS = 'DIRECT' )

    1 CONTINUE

C     Read next block
C     Note: for FORTRAN direct I/O first block/record number is 1
C           for GREAT data format first (encoded) block/record number is 0

      block_number = block_number + 1

      READ( 10, REC = block_number, iostat=io_error, ERR=1000 ) buffer

      DO block_ptr = 7, data_block( 6 ) / 4 + 4, 2


       i4 = data_block( block_ptr )
       j4 = data_block( block_ptr + 1 )

       tag = AND( RSHIFT( i4, 30 ), Z'00000003' )

       module = 0
       type = 0

C      tag = 3 = ADC data format
C      tag = 2 = Other data
C      tag = 1 = Sample trace buffer format
C      tag = 0 = undefined?

C----67---------------------------------------------------------------72------80
C     ADC data

       IF ( tag.EQ.3 ) THEN

        fail = AND( RSHIFT( i4, 29 ), Z'00000001' )

        IF ( fail.NE.0 ) THEN

         WRITE( 6, 9600 ) block_number, block_ptr, i4

        ENDIF

        range = AND( RSHIFT( i4, 28 ), Z'00000001' )
        channel_ident = AND( RSHIFT( i4, 16 ), Z'00000fff' )
        module = AND( RSHIFT( channel_ident, 6 ), Z'0000003f' )
        IF ( range.EQ.1 ) THEN
         items(module, 19 ) = items(module, 19 ) + 1
        ENDIF
        channel = AND( channel_ident, Z'0000003f' )
        adc_data = AND( i4, Z'0000ffff' )

        items(module, 0 ) = items(module, 0 ) + 1

        ts28 = AND( data_block( block_ptr + 1 ), Z'0fffffff' )

        ts = timestamp( ts48, ts28, type )

	IF ( ts.LT.ts_old ) THEN

	 items(module, 20 ) = items(module, 20 ) + 1

         WRITE( 6, 9710 ) block_number, block_ptr, i4, module, fail,
     +   range, channel_ident, channel, adc_data, ts, ts_old, tag_old,
     +   type_old
        ENDIF

        ts_old = ts
        tag_old = tag
        type_old = 0

        IF ( verbose ) THEN 
         WRITE( 6, 9700 ) block_number, block_ptr, i4, module, fail,
     +   range, channel_ident, channel, adc_data, ts
        ENDIF

        ch = channel + ( module - 1 ) * 64 + ( range * 2048 )

C----67---------------------------------------------------------------72------80
C     Other information

       ELSEIF( tag.EQ.2 ) THEN

        module = AND( RSHIFT( i4, 24 ), Z'0000003f' )
        type = AND( RSHIFT( i4, 20 ), Z'0000000f' )
        information = AND( i4, Z'000fffff' )

        items(module, 1 ) = items(module, 1 ) + 1

        ts28 = AND( data_block( block_ptr + 1 ), Z'0fffffff' )

        IF ( type.EQ.2 ) THEN

C----67---------------------------------------------------------------72------80
C     PAUSE timestamp

         items(module, 4 ) = items(module, 4 ) + 1

C         ts48 = information

        ts = timestamp( ts48, ts28, type )

         IF ( ts.LT.ts_old ) THEN

          items(module, 21 ) = items(module, 21 ) + 1

         ENDIF
         ts_old = ts
         type_old = type
         tag_old = tag
         pause( module ) = ts

         IF ( verbose ) THEN 
          WRITE( 6, 9800 ) block_number, block_ptr, i4, module, type,
     +     information, ts
         ENDIF

        ELSEIF( type.EQ.3 ) THEN

C----67---------------------------------------------------------------72------80
C     RESUME timestamp

         items(module, 5 ) = items(module, 5 ) + 1

C         ts48 = information

        ts = timestamp( ts48, ts28, type )

         IF ( ts.LT.ts_old ) THEN

          items(module, 22 ) = items(module, 22 ) + 1

         ENDIF
         ts_old = ts
         type_old = type
         tag_old = tag
         resume( module ) = ts

         IF ( resume( module ).GE.0 
     +         .AND.
     +        pause( module ).GE.0 
     +          .AND.
     +        resume( module ).GE.pause( module ) ) THEN
          dead( module ) = ( resume( module ) - pause( module ) ) 
     +                     + dead( module )
          pause( module ) = -1
          resume( module ) = -1
         ENDIF

         IF ( verbose ) THEN 
          WRITE( 6, 9810 ) block_number, block_ptr, i4, module, type,
     +     information, ts
         ENDIF

        ELSEIF( type.EQ.4 ) THEN

C----67---------------------------------------------------------------72------80
C     SYNC100 timestamp

         items(module, 6 ) = items(module, 6 ) + 1

         ts48 = information
         ts = timestamp( ts48, ts28, type )

         IF ( first_ts.EQ.0 ) THEN
          first_ts = ts 
         ENDIF
 
         IF ( ts.LT.ts_old ) THEN

          items(module, 23 ) = items(module, 23 ) + 1

          WRITE( 6, 9825 ) block_number, block_ptr, i4, module, type,
     +     information, ts, ts_old, tag_old, type_old

         ENDIF
         ts_old = ts
         type_old = type
         tag_old = tag

C     Estimate FEE 'idle' time by integrating periods > 400000 (hex) = 4194304 (dec)
C     clock cycles (= 0.04194304s) between successive SYNC100s from given FEE module.
C     This period corresponds to 16 successive SYNC100s, 

         IF ( sync( module ).GT.0 ) THEN
          IF ( ts - sync( module ) .GT. X'400000' ) THEN
           idle( module ) = idle( module ) + ( ts - sync( module ) )
          ENDIF
         ENDIF

         sync( module ) = ts

         IF ( verbose ) THEN 
          WRITE( 6, 9820 ) block_number, block_ptr, i4, module, type,
     +     information, ts
         ENDIF

        ELSEIF( type.EQ.6 ) THEN

C----67---------------------------------------------------------------72------80
C     FEE64 discriminator

         items(module, 7 ) = items(module, 7 ) + 1

        ts = timestamp( ts48, ts28, type )

         IF ( ts.LT.ts_old ) THEN
 
         items(module, 24 ) = items(module, 24 ) + 1
 
          WRITE( 6, 9835 ) block_number, block_ptr, i4, module, type,
     +     information, ts, ts_old, tag_old, type_old
         ENDIF
         ts_old = ts
         type_old = type
         tag_old = tag

         IF ( verbose ) THEN 
          WRITE( 6, 9830 ) block_number, block_ptr, i4, module, type,
     +     information, ts
         ENDIF

        ELSEIF( type.EQ.8 ) THEN

C----67---------------------------------------------------------------72------80
C     MBS information

         items(module, 8 ) = items(module, 8 ) + 1

        ts = timestamp( ts48, ts28, type )

         IF ( ts.LT.ts_old ) THEN
 
         items(module, 25 ) = items(module, 25 ) + 1
 
        ENDIF
         ts_old = ts
         type_old = type
         tag_old = tag

         information_index = AND( RSHIFT(information,16), Z'0000000f' )
         mbs_data = AND( information, Z'0000ffff' )
         IF ( verbose ) THEN 
          WRITE( 6, 9840 ) block_number, block_ptr, i4, module, type,
     +     information_index, mbs_data, ts
         ENDIF

        ELSE

C----67---------------------------------------------------------------72------80
C     Something else ...

        items(module, 9 ) = items(module, 9 ) + 1

        ts = timestamp( ts48, ts28, type )

        IF ( ts.LT.ts_old ) THEN
          items(module, 26 ) = items(module, 26 ) + 1
        ENDIF
         ts_old = ts
         type_old = type
         tag_old = tag
       
         IF ( verbose ) THEN 
          WRITE( 6, 9850 ) block_number, block_ptr, i4, module, type,
     +     information, ts
         ENDIF

        ENDIF

C----67---------------------------------------------------------------72------80
C     Sample data trace buffer

       ELSEIF( tag.EQ.1 ) THEN

        channel_ident = AND( RSHIFT( i4, 16 ), Z'00000fff' )
        module = AND( RSHIFT( channel_ident, 6 ), Z'0000003f' )
        channel = AND( channel_ident, Z'0000003f' )
        sample_length = AND( i4, Z'0000ffff' )

        items(module, 2 ) = items(module, 2 ) + 1

        ts28 = AND( data_block( block_ptr + 1 ), Z'0fffffff' )

        ts = timestamp( ts48, ts28, type )

        IF ( ts.LT.ts_old ) THEN

         items(module, 27 ) = items(module, 27 ) + 1

        ENDIF
        ts_old = ts
        tag_old = tag
        type_old = 0

        IF ( verbose ) THEN 

         WRITE( 6, 9900 ) block_number, block_ptr, i4, channel_ident,
     +    module, channel, sample_length, ts
     
        DO i = block_ptr+2, ( block_ptr+2 ) + sample_length / 2 - 1, 2

C         WRITE( 6, 9950 ) AND( RSHIFT(data_block(i),16), Z'00003fff' ),
C     +    AND( data_block(i), Z'00003fff' ),
C     +    AND( RSHIFT(data_block(i+1),16), Z'00003fff' ),
C     +    AND( data_block(i+1), Z'00003fff' )

C        block_ptr = block_ptr + sample_length / 2 - 1

        ENDDO
        ENDIF

C----67---------------------------------------------------------------72------80
C     Undefined tag

       ELSE

        items(module, 3 ) = items(module, 3 ) + 1

        IF ( verbose ) THEN 
         WRITE( 6, * ) ' *** ERROR: undefined tag: tag:', tag,
     +                ' block:', block_number, ' ptr:', block_ptr
        ENDIF

       ENDIF

C----67---------------------------------------------------------------72------80
C 
      ENDDO

C     Return to read next block

      GOTO 1

C     End of file

 1000 CONTINUE

      IF ( io_error.NE.0 ) THEN
       WRITE( 6, 9020 ) io_error
       block_number = block_number - 1
      ENDIF

C     Close file

      CLOSE( 10 )

C     Output statistics

      dt = DFLOAT( ts - first_ts ) * 10.0D-09

      DO i = 0, 31
       DO j = 0, 32
        sum( i ) = sum( i ) + items( j, i )
       ENDDO
      ENDDO

      WRITE( 6, 9400 ) block_number, ( sum( i ),
     +                 DFLOAT( sum( i ) )/ dt, i = 0, 9 )
      WRITE( 6, 9410 ) ( sum( i ),
     +                 DFLOAT( sum( i ) )/ dt, i = 19, 27 )
      WRITE( 6, 9420 ) dt   
      WRITE( 6, 9425 )
      DO i = 1,32 
       WRITE( 6, 9430 ) i, DFLOAT( dead( i ) ) * 10.0D-09,
     +                     DFLOAT( idle( i ) ) * 10.0D-09
      ENDDO
      WRITE( 6, 9440 )
      DO i = 0, 32
       WRITE( 6, 9450 ) i, ( items( i, j ), j = 0, 9 ), items( i, 19 )
      ENDDO
      WRITE( 6, 9460 )
      DO i = 0, 32
       WRITE( 6, 9470 ) i, ( items( i, j ), j = 20, 27 )
      ENDDO  

C     Elapsed time etc

      t2 = SECNDS( t1 )

      WRITE( 6, 9300 ) t2, block_number/t2,
     +                 block_number*64.0/(1024.0*t2)

      STOP

C----67---------------------------------------------------------------72------80

 9000 FORMAT( ' *** TDR format 3.3.0 analyser - TD - October 2016' )
 9020 FORMAT( ' *** ERROR: READ I/O error: ', i10 )
 9010 FORMAT( ' *** ERROR: OPEN I/O error: ', i10 )
 9030 FORMAT( ' *** ERROR: CLOSE I/O error: ', i10 )

 9300 FORMAT(/' *** Program elapsed time:', F9.3, 's (', F9.3,
     +        ' blocks/s,', F8.3, ' Mb/s)' )

 9400 FORMAT( '                   blocks: ', i10,
     +       /'          ADC data format: ', i10, ' (', F10.1,' Hz)', 
     +       /'        Other data format: ', i10, ' (', F10.1,' Hz)', 
     +       /' Sample trace data format: ', i10, ' (', F10.1,' Hz)', 
     +       /'         Undefined format: ', i10, ' (', F10.1,' Hz)', 
     +       /'   Other data format type:      PAUSE: ', i10,
     +        ' (', F10.1,' Hz)', 
     +       /'                               RESUME: ', i10,
     +        ' (', F10.1,' Hz)', 
     +       /'                              SYNC100: ', i10,
     +        ' (', F10.1,' Hz)', 
     +       /'                           FEE64 disc: ', i10,
     +        ' (', F10.1,' Hz)', 
     +       /'                             MBS info: ', i10,
     +        ' (', F10.1,' Hz)',  
     +       /'                           Other info: ', i10,
     +        ' (', F10.1,' Hz)'  )
 9410 FORMAT(/'   ADC data range bit set: ', i10,' (', F10.1,' Hz)',
     +      //'                Timewarps:        ADC: ', i10,
     +        ' (', F10.1,' Hz)',
     +       /'                                PAUSE: ', i10,
     +        ' (', F10.1,' Hz)',
     +       /'                               RESUME: ', i10,
     +        ' (', F10.1,' Hz)',
     +       /'                              SYNC100: ', i10,
     +        ' (', F10.1,' Hz)',
     +       /'                           FEE64 disc: ', i10,
     +        ' (', F10.1,' Hz)',
     +       /'                             MBS info: ', i10,
     +        ' (', F10.1,' Hz)',
     +       /'                            Undefined: ', i10,
     +        ' (', F10.1,' Hz)',
     +       /'                         Sample trace: ', i10,
     +        ' (', F10.1,' Hz)' )
 9420 FORMAT(/' *** Timestamp elapsed time: ', F12.3, ' s' )
 9425 FORMAT( ' FEE  elapsed dead time(s) elapsed idle time(s)' )
 9430 FORMAT( 1x, i2, 1x, F20.3, 1x, F20.3 )
C----67---------------------------------------------------------------72------80
 9440 FORMAT(/' *** Statistics',
     +       /' FEE  ADC Data Other Data     Sample',
     +        '  Undefined      Pause     Resume    SYNC100',
     +        '       Disc        MBS      Other   HEC Data' )
 9450 FORMAT( 1x, i2, 11( 1x, i10 ) )
 9460 FORMAT(/' *** Timewarps',
     +       /' FEE       ADC      Pause     Resume    SYNC100',
     +        '       Disc        MBS  Undefined    Samples' )
 9470 FORMAT( 1x, i2, 8( 1x, i10 ) )

 9500 FORMAT( ' ptr:', i6, ' data:', Z8.8, 1x, z8.8 )

 9600 FORMAT( ' *** ERROR: fail bit set: block:', i6, ' ptr:', i6,
     +    ' data:', Z8.8 )

 9700 FORMAT( ' ***            ADC data: block:', i6, ' ptr:', i6,
     +    ' data: 0x', Z8.8, ' module:', i3,' fail:', i2, ' range:', i2,
     +    ' id:', i6, ' channel:', i3,' adc:', i6, '  ts: 0x', Z12.12 )


 9710 FORMAT( ' ***            ADC data: block:', i6, ' ptr:', i6,
     +   ' data: 0x', Z8.8, ' module:', i3,' fail:', i2, ' range:', i2,
     +    ' id:', i6, ' channel:', i3,' adc:', i6, '  ts: 0x', Z12.12,
     +    ' OLD: ts: 0x', Z12.12, ' tag:', i2, ' type:', i2 )

 9800 FORMAT( ' ***     PAUSE timestamp: block:', i6, ' ptr:', i6,
     +    ' data: 0x', Z8.8, ' module:', i3, ' information type:', i3,
     +    ' information field: 0x', Z8.8, ' ts: 0x', Z12.12 )

 9810 FORMAT( ' ***    RESUME timestamp: block:', i6, ' ptr:', i6,
     +    ' data: 0x', Z8.8, ' module:', i3, ' information type:', i3,
     +    ' information field: 0x', Z8.8, ' ts: 0x', Z12.12 )

 9820 FORMAT( ' ***   SYNC100 timestamp: block:', i6, ' ptr:', i6,
     +    ' data: 0x', Z8.8, ' module:', i3, ' information type:', i3,
     +    ' information field: 0x', Z8.8, ' ts: 0x', Z12.12 )

 9825 FORMAT( ' ***   SYNC100 timestamp: block:', i6, ' ptr:', i6,
     +    ' data: 0x', Z8.8, ' module:', i3, ' information type:', i3,
     +    ' information field: 0x', Z8.8, ' ts: 0x', Z16.16,
     +    ' OLD: ts: 0x', Z16.16 ' tag:', i2, ' type:', i2 )

 9830 FORMAT( ' *** FEE64 discriminator: block:', i6, ' ptr:', i6,
     +    ' data: 0x', Z8.8, ' module:', i3, ' information type:', i3,
     +    ' information field: 0x', Z8.8, ' ts: 0x', Z12.12 )


 9835 FORMAT( ' *** FEE64 discriminator: block:', i6, ' ptr:', i6,
     +    ' data: 0x', Z8.8, ' module:', i3, ' information type:', i3,
     +    ' information field: 0x', Z8.8, ' ts: 0x', Z12.12,
     +    ' OLD: ts: 0x', Z12.12 ' tag:', i2, ' type:', i2 )

 9840 FORMAT( ' ***     MBS information: block:', i6, ' ptr:', i6,
     +    ' data: 0x', Z8.8, ' module:', i3, ' information type:', i3,
     +    ' information index:', i3, ' MBS data:', Z4.4,
     +    ' ts: 0x', Z12.12 )

 9850 FORMAT( ' block:', i6, ' ptr:', i6, ' data: 0x', Z8.8,
     +    ' module:', i3, ' information type:', i3,
     +    ' information field: 0x', Z8.8, ' ts: 0x', Z12.12 )

 9900 FORMAT( ' block:', i6, ' ptr:', i6, ' data: 0x', Z8.8, ' id:', i6,
     +   ' module:', i3, ' channel:', i3,' sample length:', i6,
     +   ' ts: 0x', Z12.12 )

 9950 FORMAT( 4( 1x, Z8.8 ) )

C----67---------------------------------------------------------------72------80

      END

C----67---------------------------------------------------------------72------80

      INTEGER*8 FUNCTION timestamp( ts48, ts28, type )

      INTEGER lshift
C
C     Argument variables
C
      INTEGER ts28, ts48, type
C
C     Local variables
C
      INTEGER i4_2(2)
      INTEGER*8 i8

      EQUIVALENCE ( i8, i4_2(1) )
      
      timestamp = 0
      i8 = 0
      IF ( (type.LT.2 .OR. type.GT.4) .AND. ts28.LE.Z'00000A0' ) THEN
       i4_2( 1 ) = ts48 + 1
      ELSE
       i4_2( 1 ) = ts48
      ENDIF
      timestamp = LSHIFT( i8, 28 )
      i8 = 0
      i4_2(1) = ts28
      timestamp = timestamp + i8

      RETURN

C----67---------------------------------------------------------------72------80

      END

C----67---------------------------------------------------------------72------80
