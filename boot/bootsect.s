!
! SYS_SIZE is the number of clicks (16 bytes) to be loaded.
! 0x3000 is 0x30000 bytes = 196kB, more than enough for current
! versions of linux
!SYS SIZE是要加载的节数(16字节为1节)。0x3000共为
!0x30000字节=196kB(若以1024字节为1B计，则应该是192KB),对于当前的版本空间已足多了
!
!指编译连接后system模块的大小。这里给出了一个最大默认值
SYSSIZE = 0x3000
!
!	bootsect.s		(C) 1991 Linus Torvalds
!
! bootsect.s is loaded at 0x7c00 by the bios-startup routines, and moves
! iself out of the way to address 0x90000, and jumps there.
!
! It then loads 'setup' directly after itself (0x90200), and the system
! at 0x10000, using BIOS interrupts. 
!
! NOTE! currently system is at most 8*65536 bytes long. This should be no
! problem, even in the future. I want to keep it simple. This 512 kB
! kernel size should be enough, especially as this doesn't contain the
! buffer cache as in minix
!
! The loader has been made as simple as possible, and continuos
! read errors will result in a unbreakable loop. Reboot by hand. It
! loads pretty fast by getting whole sectors at a time whenever possible.
!以下是前面这些文字的翻译：
!	bootsect.s		(C)1991 Linus Torvalds版权所有
!
!bootsect.s被bios-启动子程序加载至0x7c00(31k)处，并将自己
!移到了地址0x90000(576k)处，并跳转至那里。
!
!它然后使用BI0S中断将'setup'直接加载到自己的后面(0x90200)(576.5k),
!并将system加载到地址0x10000处。
!
!注意！日前的内核系统最大长度限制为(8*65536)(512k)字节，即使是在
!将来这也应该没有问题的。我想让它保持简单明了。这样512k的最大内核长度应该
!足够了，尤其是这里没有象minix中一样包含缓冲区高速缓冲。
! 
 
!加载程序已经做的够简单了，所以持续的读出错将导致死循环。只能手工重启。
!只要可能，通过一次读取所有的扇区，加载过程可以做的很快。
.globl begtext, begdata, begbss, endtext, enddata, endbss	!定义了6个全局标识符：
.text	!文本段
begtext:	
.data	!数据段
begdata:
.bss	!未初始化数据段(Block Started by Symbol)
begbss:
.text	!文本段

SETUPLEN = 4				! nr of setup-sectors setup程序的删除数(setup-sectors)值
BOOTSEG  = 0x07c0			! original address of boot-sector bootsect的原始地址(段地址)
INITSEG  = 0x9000			! we move boot here - out of the way 将bootsect移到这里
SETUPSEG = 0x9020			! setup starts here setup程序从这里开始
SYSSEG   = 0x1000			! system loaded at 0x10000 (65536). system模块加载到0x10000(64kb)处
ENDSEG   = SYSSEG + SYSSIZE		! where to stop loading 停止加载的段地址

! ROOT_DEV:	0x000 - same type of floppy as boot.
!					根文件系统设备使用与引导时同样的软驱设备
!		0x301 - first partition on first drive etc
!				根文件系统设备在第一硬盘的第一个分区上,等等

!指定根文件系统设备是第2个硬盘的第1个分区。这是Liux老式的硬盘命名
!方式，具体值的含义如下：
!设备号=主设备号*256+次设备号（他即devn0=(major<<8)+minor)
!(主设备号：1-内存，2-磁盘，3-硬盘，4-ttyx,5-tty,6-并行口，7-非命名管道)
!0x300-/dev/hd0-代表整个第1个硬盘：
!0x301-/dev/hd1-第1个盘的第1个分区：
!...
!0x304-/dev/hd4-第1个盘的第4个分区：
!0x305-/dev/hd5-代表整个第2个硬盘：
!0x306-/dev/hd6-第2个盘的第1个分区：
!...
!0x309-/dev/hd9-第2个盘的第4个分区；
!从1iux内核0.95版后已经使用与现在相同的命名方法了。
ROOT_DEV = 0x306

entry _start		! 告知链接程序,程序从start标号开始执行
_start:				!83--92行作用是将自身(bootsect)从目前段位置0x07c0(31k)
					!移动到0x9000(576k)处，共256字(512字节)，然后跳转到
					!移动后代码的g0标号处，也即本程序的下一语句处。
	mov	ax,#BOOTSEG
	mov	ds,ax		!将ds设置为0x7c0
	mov	ax,#INITSEG
	mov	es,ax		!将es设置为0x9000
	mov	cx,#256		!移动计数值=256字
	sub	si,si		!源地址	ds:si = 0x07c0:0x0000
	sub	di,di		!目的地址 es:di = 0x9000:0x0000
	rep				!重复执行,直到cx = 0
	movw			!移动1个字
	jmpi	go,INITSEG	!间接跳转,这里INITSEG指出跳转到的段地址
						!从下面开始,CUP执行已移动到0x9000段处的代码
go:	mov	ax,cs			!将ds、es和ss都设置成移动后代码所在的段处(0x9000)
	mov	ds,ax			
	mov	es,ax
! put stack at 0x9ff00.
	mov	ss,ax			!由于程序中有堆栈操作(push,pop,call),因此必须设置堆栈
	mov	sp,#0xFF00		! arbitrary value >>512 将堆栈指针sp指向0x9ff00(即0x9000:0xff00)处
						!由于代码段移动过了，所以要重新设置堆栈段的位置。
						!sp只要指向远大于512偏移（即地址0x90200)处
						!都可以。因为从0x90200地址开始处还要放置setup程序，
						!而此时setup程序大约为4个扇区，因此sp要指向大
						!于(0x200+0x200*4+堆栈大小)处。
! load the setup-sectors directly after the bootblock.
! Note that 'es' is already set up.
! 在bootsect程序块后紧根着加载setup模块的代码数据。
! 注意es已经设置好了。（在移动代码时es已经指向目的段地址处0x9000)

load_setup:
!68-77行的用途是利用BI0S中断IT0x13将setup模块从磁盘第2个扇区
!开始读到0x90200开始处，共读4个扇区。如果读出错，则复位驱动器，并
!重试，没有退路。INT0x13的使用方法如下：
!读扇区：
!ah=0x02-读磁盘扇区到内存；al=需要读出的扇区数量：
!ch=磁道（柱面）号的低8位；c1=开始扇区(0-5位)，磁道号高2位(6-7)
!dh=磁头号：d1=驱动器号（如果是硬盘则位7要置位）：
!es:bx→指向数据缓冲区；如果出错则CF标志置位。
	mov	dx,#0x0000		! drive 0, head 0
	mov	cx,#0x0002		! sector 2, track 0
	mov	bx,#0x0200		! address = 512, in INITSEG
	mov	ax,#0x0200+SETUPLEN	! service 2, nr of sectors
	int	0x13			! read it
	jnc	ok_load_setup		! ok - continue
	mov	dx,#0x0000
	mov	ax,#0x0000		! reset the diskette
	int	0x13
	j	load_setup

ok_load_setup:

! Get disk drive parameters, specifically nr of sectors/track
!取磁盘驱动器的参数，特别是每道的扇区数量。
!取磁盘驱动器参数INT0x13调用格式和返回信息如下：
!ah=0x08	d1=驱动器号（如果是硬盘则要置位7为1）。
!返回信息：
!如果出错则CF置位，并且ah=状态码。
!ah=0,a1=0,		b1=驱动器类型(AT/PS2)
!ch=最大磁道号的低8位，c1=每磁道最大扇区数（位0-5），最大磁道号高2位（位6-7）
!dh=最大磁头数，	d1=驱动器数量，
!es:di-→软驱磁盘参数表。
	mov	dl,#0x00
	mov	ax,#0x0800		! AH=8 is get drive parameters
	int	0x13
	mov	ch,#0x00
	seg cs				!表示下一条语句的操作数在cs段寄存器所指的段中
	mov	sectors,cx		!保存每磁道扇区数
	mov	ax,#INITSEG
	mov	es,ax			!因为上面取磁盘参数中断改掉了es的值,这里重新改回来

! Print some inane message 显示一些信息('Loading system ...')回车换行共24个字符

	mov	ah,#0x03		! read cursor pos 读光标位置
	xor	bh,bh
	int	0x10
	
	mov	cx,#24			!共24个字符
	mov	bx,#0x0007		! page 0, attribute 7 (normal)
	mov	bp,#msg1		!指向要显示的字符串
	mov	ax,#0x1301		! write string, move cursor 写字符串并移动光标
	int	0x10

! ok, we've written the message, now
! we want to load the system (at 0x10000) 现在开始将system模块加载到0x10000(64k)处

	mov	ax,#SYSSEG
	mov	es,ax		! segment of 0x010000 es = 存放system的段地址
	call	read_it	!读磁盘上system模块,es为输入参数
	call	kill_motor	!关闭驱动器马达,这样就可以知道驱动器的状态了

! After that we check which root-device to use. If the device is
! defined (!= 0), nothing is done and the given device is used.
! Otherwise, either /dev/PS0 (2,28) or /dev/at0 (2,8), depending
! on the number of sectors that the BIOS reports currently.
!此后，我们检查要使用哪个根文件系统设备（简称根设备）。如果已经指定了设备(！=0)
!就直接使用给定的设备。否则就需要根据BIOS报告的每磁道扇区数来
!确定到底使用/dev/PS0(2,28)还是/dev/at0(2,8)。
!上面一行中两个设备文件的含义：
!在Linux中软驱的主设备号是2（参见第43行的注释），次设备号=type*4+nr,其中
!nr为0-3分别对应软驱A、B、C或D;type是软驱的类型(2→1.2M或7→1.44M等)。
!因为7*4+0=28，所以/dev/PS0(2,28)指的是1.44MA驱动器，其设备号是0x021c
!同理/dev/at0(2,8)指的是1.2MA驱动器，其设备号是0x0208。
	seg cs
	mov	ax,root_dev		!将根设备号
	cmp	ax,#0
	jne	root_defined
	seg cs
	mov	bx,sectors		!取上面第147行保存的每磁道扇区数,如果sectors=15则说明是1.2Mb的驱动器；
						!如果sectors=18则说明是1.44Mb软驱。因为是可引导的驱动器,所以肯定是A驱

	mov	ax,#0x0208		! /dev/ps0 - 1.2Mb
	cmp	bx,#15			!判断每磁道扇区数是否=15
	je	root_defined	!如果等于则ax中就是引导驱动器的设备号
	mov	ax,#0x021c		! /dev/PS0 - 1.44Mb
	cmp	bx,#18
	je	root_defined
undef_root:				!如果都不一样,则死循环(死机)
	jmp undef_root
root_defined:
	seg cs
	mov	root_dev,ax		!将检查过的设备号保存起来

! after that (everyting loaded), we jump to
! the setup-routine loaded directly after
! the bootblock:
!到此，所有程序都加载完毕,我们就跳转到被加载在bootsect后面的setup程序去
	jmpi	0,SETUPSEG
!下面是两个子程序
! This routine loads the system at address 0x10000, making sure
! no 64kB boundaries are crossed. We try to load it as fast as
! possible, loading whole tracks whenever we can.
!
! in:	es - starting address segment (normally 0x1000)
!
!该子程序将系统模块加载到内存地址0x10000处，并确定没有跨越64B的内存边界。我们试图尽快
!地进行加载，只要可能，就每次加载整条磁道的数据。
!输入：es-开始内存地址段值（通常是0x1000)
sread:	.word 1+SETUPLEN	! sectors read of current track 
							!当前磁道中已读取的扇区数,开始时已经读进1扇区的引导扇区
							!bootsect和setup程序所占的扇区数SETUPLEN
head:	.word 0			! current head	当前磁头号
track:	.word 0			! current track	当前磁道号

read_it:
!测试输入的段值。从盘上读入的数据必须存放在位于内存地址64B的边界开始处，否则进入死循环。
!清bx寄存器，用于表示当前段内存放数据的开始位置。
	mov ax,es
	test ax,#0x0fff
die:	jne die			! es must be at 64kB boundary es值必需位于64KB地址边界
	xor bx,bx		! bx is starting address within segment	bx为段内偏移位置
rp_read:
!判断是否已经读入全部数据。比较当前所读段是否就是系统数据末端所处的段(#DSEG),如果不是就
!跳转至下面ok1read标号处继续读数据。否则退出子程序返回。
	mov ax,es
	cmp ax,#ENDSEG		! have we loaded all yet? 是否已经加载了全部数据?
	jb ok1_read
	ret
ok1_read:
!计算和验证当前磁道需要读取的扇区数，放在ax寄存器中。
!根据当前磁道还未读取的扇区数以及段内数据字节开始偏移位置，计算如果全部读取这些未读扇区，
!读总字节数是否会超过64B段长度的限制。若会超过，则根据此次最多能读入的字节数(64B-段
!偏移位置),反算出此次需要读取的扇区数
	seg cs
	mov ax,sectors		!取每磁道扇区数
	sub ax,sread		!减去当前磁道已读扇区数
	mov cx,ax			!cx = ax = 当前磁道未读扇区数
	shl cx,#9			!cx = cx * 512字节
	add cx,bx			!bx = cx + 段内当前偏移值(bx) = 此次读操作后,段内共读入的字节数
	jnc ok2_read		!若没有超过64KB字节,则跳转至ok2_read处执行
	je ok2_read
	xor ax,ax			!若加上此次将读磁道上所有未读扇区时会超过64KB,则计算
	sub ax,bx			!此时最多读入的字节数(64KB - 段内偏移位置),再转换
	shr ax,#9			!成需要读取的扇区数
ok2_read:
!读当前磁道上指定开始扇区(cl)和需读扇区数(al)的数据到es:bx开始处。然后统计当前磁道
!上已经读取的扇区数并与磁道最大扇区数sectors作比较。如果小于sectors说明当前磁道上的还
!有扇区未读。于是跳转到ok3read处继续操作。
	call read_track		!读当前磁道上指定开始扇区数和需读扇区数的数据
	mov cx,ax			!cx = 该次操作已读取的扇区数
	add ax,sread		!加上当前磁道上已经读取的扇区数
	seg cs
	cmp ax,sectors		!如果当前磁道上还有扇区未读,则跳转到ok3_read
	jne ok3_read
!若该磁道的当前磁头面所有扇区已经读取,则读该磁道的下一磁头面(1号磁头)上的数据。如果已经完成,则去读下一磁道
	mov ax,#1
	sub ax,head			!判单当前磁头号
	jne ok4_read		!如果是0磁头,则再去读1磁头面上的扇区数据
	inc track			!否则去读下一磁道
ok4_read:
	mov head,ax			!保存当前磁头号
	xor ax,ax			!清当前磁道已读扇区数
ok3_read:
!如果当前磁道上的还有未读扇区，则首先保存当前磁道已读扇区数，然后调整存放数据处的开始
!位置。若小于64KB边界值，则跳转到rp read(231行)处，继续读数据。
	mov sread,ax		!保存当前磁道已读扇区数
	shl cx,#9			!上次已读扇区数*512字节
	add bx,cx			!调整当前段内数据开始位置
	jnc rp_read
!否则说明已经读取64KB数据。此时调整当前段,为读下一段数据做准备
	mov ax,es
	add ax,#0x1000		!将段基址调整为指向下一个64KB内存开始处
	mov es,ax
	xor bx,bx			!清除当前段内数据开始位置
	jmp rp_read			!跳转至rp_read(231行)处,继续读数据

!read_track子程序。
!读当前磁道上指定开始扇区和需读扇区数的数据到es:bx开始处。参见第110行下对BIOS磁盘读中断
!int0xl3,ah=2的说明。
!al-需读扇区数；es:bx-缓冲区开始位置。
read_track:
	push ax
	push bx
	push cx
	push dx
	mov dx,track		!取当前磁道号
	mov cx,sread		!取当前磁道上已读扇区数
	inc cx				!cl = 开始读扇区
	mov ch,dl			!ch = 当前磁道号
	mov dx,head			!取当前磁头号
	mov dh,dl			!dh = 磁头号
	mov dl,#0			!dl = 驱动器号(为0表示当前A驱动器)
	and dx,#0x0100		!磁头号不大于1
	mov ah,#2			!ah = 2,读磁盘扇区功能号
	int 0x13
	jc bad_rt			!若出错,则跳转至bad_rt
	pop dx
	pop cx
	pop bx
	pop ax
	ret
!读磁盘操作出错。则执行驱动器复位操作（磁盘中断功能号0），再跳转到read track处重试。
bad_rt:	mov ax,#0
	mov dx,#0
	int 0x13
	pop dx
	pop cx
	pop bx
	pop ax
	jmp read_track

!/*
! * This procedure turns off the floppy drive motor, so
! * that we enter the kernel in a known state, and
! * don't have to worry about it later.
! */
!/*这个子程序用于关闭软驱的马达，这样我们进入内核后就能
! *知道它所处的状态，以后也就无须担心它了。
! */
!下面第336行上的值0x3f2是软盘控制器的一个端口，被称为数字输出寄存器(D0)端口。它是
!一个8位的寄存器，其位7--位4分别用于控制4个软驱(D-A)的启动和关闭。位3-位2用于
!允许/禁止DMA和中断请求以及启动/复位软盘控制器FDC。位1-位0用于选择选择操作的软驱。
!第337行上在al中设置并输出的0值，就是用于选择A驱动器，关闭FDC,禁止DMA和中断请求，
!关闭马达。有关软驱控制卡编程的详细信息请参见kernel/blk_drv/floppy.c程序后面的说明。
kill_motor:
	push dx
	mov dx,#0x3f2		!软驱控制卡的数字输出寄存器(DOR)端口,只写
	mov al,#0			!A驱动器,关闭FDC,禁止DMA和终端请求,关闭马达
	outb				!将al中的内容输出到dx指定的端口去
	pop dx
	ret

sectors:
	.word 0				!存放当前启动软盘每磁道的扇区数

msg1:					!调用BIOS中断显示的信息
	.byte 13,10			!回车、换行的ASCII码
	.ascii "Loading system ..."
	.byte 13,10,13,10	!共24个ASCII码

!表示下面语句从地址508(0x1FC)开始，所以root_dev在启动扇区的第508开始的2个字节中。
.org 508
root_dev:
	.word ROOT_DEV		!这里存放根文件系统所在设备号(init/main.c中会用)

!下面是启动盘具有有效引导扇区的标志。仅供BIOS中的程序加载引导扇区时识别使用。它必须位于
!引导扇区的最后两个字节中。
boot_flag:
	.word 0xAA55

.text
endtext:
.data
enddata:
.bss
endbss:
