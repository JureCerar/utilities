! Debug moode
#ifndef _DEBUG
#define _DEBUG .false.
#endif
! Program name
#define _PROGRAM "tpl2ndx"
! Program version
#ifndef _VERSION
#define _VERSION "v0.0.0"
#endif

program main
	use xslib
	implicit none
	type(tpl_file)	:: tpl
	type(ndx_file)	:: ndx
	character*512	:: inFile="", outFile="index.ndx"
	integer			:: i, j, n, dmy, next, offset, stat
	logical 		:: atoms=.false., mols=.false.

	! Get external options
	call getOpts ()

	inFile="example/temp.tpl"
	outFile="example/temp.ndx"

	! Read .tpl file
	call tpl%read(inFile)

	! ---------------------------
	! Group 1 'System' (all)
	! Group 2 'Molecule X' (molecule)
	! ....
	! Group 3 'Atom X' (atoms)
	! ....


	! ---------------------------
	! Modify IDs if neccessery
	if (atoms) then
		! Each atom gets its unique ID
		do i = 1, tpl%natoms
			tpl%id(i) = i

		end do

	else if (mols) then
		! Each molecule gets its unique ID
		do n = 1, tpl%ntypes
			tpl%type(n)%id(:) = n

		end do

	end if

	if (_DEBUG) call tpl%write()

	! Transform tpl to ndx
	call tpl2ndx (tpl, ndx)

	! ------------------------------------------------------------------------
	! Write all groups to STDOUT just to check

	do n = 1, ndx%ngroups
		write (*,"(a)") "Group "//str(n)//" '"//ndx%group(n)%title//"' ("// &
		str(ndx%group(n)%natoms)//" atoms)"

	end do

	! --------------------------
	! Write output

	! Backup any existing index files
	call backup(outFile)

	write (*,"(a)") "Writing output to: "//trim(outFile)
	call ndx%write(FILE=outFile)

	! Clean-up
	write (*,"(a)") "Task finished."

	call exit (0)
contains

	! Get external options
	subroutine getOpts ()
		use iso_fortran_env
		implicit none
		character*1024	:: arg
		integer			:: i, stat, next

		! if (command_argument_count() == 0) go to 100

		next = 0
		do while (next <= command_argument_count())
			next = next+1
			call get_command_argument (next, arg, STATUS=stat)
			if (stat/=0 .or. verify(arg, " ")==0) exit

			select case (trim(arg))
			case ("-h","--help")
				write (*,*) _PROGRAM//" [options] [input <.tpl>]"
				write (*,*)	""
				write (*,*)	"DESCRIPTION:"
				write (*,*)	"  Transform template (.tpl) to GROMACS index (.ndx) file."
				write (*,*)	"  Index group is created containing all atoms with same non-zero ID in .tpl file."
				write (*,*)	""
				write (*,*)	"OPTIONS:"
				write (*,*)	" -h, --help   -- Print this message."
				write (*,*)	" -o  [name]   -- (optional) Output file name. ("//trim(outFile)//")"
				write (*,*) " -atoms/mols  -- Create index group for each all ATOMS or MOLECULES."
				write (*,*)	""
				write (*,*) "ABOUT:"
				write (*,*) "Program:  "//_VERSION//" -- "//__DATE__//" "//__TIME__
				write (*,*) "XsLib:    "//trim(xslibINFO)
				if (_DEBUG) write (*,*) " --- DEBUG MODE --- "
				call exit (0)

			! Output file
			case ("-o")
				next = next+1
				call get_command_argument (next, arg, STATUS=stat)
				read (arg, *, IOSTAT=stat) outFile

			! Atoms 'mode'
			case ("-atoms")
				atoms=.true.

			! Molecules 'mode'
			case ("-mols")
				mols=.true.

			case default
				read (arg, *, IOSTAT=stat) inFile

			end select
		end do

		! Check if input file is present
		if (verify(inFile, " ")==0) then
			write (error_unit, "(a)") "WARNING - No input files."
			write (error_unit, "(a)") "Try '--help' flag for more information."
			write (error_unit, "(a)") _PROGRAM//" - "//_VERSION
			call exit (1)

		end if

		return
	end subroutine getOpts


	subroutine tpl2ndx (tpl, ndx)
		implicit none
		type(tpl_file) :: tpl
		type(ndx_file) :: ndx
		logical, allocatable	:: mask(:)
		integer					:: i,j,n,current, lower, upper

		! Count number of unique elements
		current = minval(tpl%id(:))-1
		ndx%ngroups = 1 ! First group is 'System'
		do
			! Find next unique element.
			current = minval(tpl%id(:), MASK=tpl%id(:)>current)
			! Zero elements do not interest us
			if (current==0) cycle
			! LOOP CONTROL; If no element can be found exit loop
			if (current==huge(current)) exit

			ndx%ngroups = ndx%ngroups+1

		end do

		! Allocate memory
		if (allocated(ndx%group)) deallocate(ndx%group, STAT=stat)
		allocate(ndx%group(ndx%ngroups), STAT=stat)

		! -----------------------------------------------------
		! Create 'System' group -- ALL PARTICLES
		next = 1

		! Create title
		ndx%group(next)%title = "System"

		! All particles in system
		ndx%group(next)%natoms = sum(tpl%type(:)%natoms*tpl%type(:)%nmol)

		! Allocate data
		if (allocated(ndx%group(next)%loc)) deallocate(ndx%group(next)%loc, STAT=stat)
		allocate(ndx%group(next)%loc(ndx%group(next)%natoms), STAT=stat)

		! Generate sequence
		ndx%group(next)%loc = [(i, i=1,ndx%group(next)%natoms)]

		! Increment global counter
		next = next+1

		! -----------------------------------------------------
		! ALL OTHER groups

		! Allocate logical mask
		if (allocated(mask)) deallocate(mask, STAT=stat)
		allocate(mask(ndx%group(1)%natoms), STAT=stat)

		current = minval(tpl%id(:))-1
		do
			current = minval(tpl%id(:), MASK=tpl%id(:)>current)
			if (current==0) cycle
			if (current==huge(current)) exit

			! Loop through all molecules
			do n = 1, tpl%ntypes
				lower = sum(tpl%type(:n-1)%natoms*tpl%type(:n-1)%nmol)+1
				upper = (lower-1)+tpl%type(n)%natoms*tpl%type(n)%nmol

				mask(lower:upper) = [((tpl%type(n)%id(i)==current, i = 1, tpl%type(n)%natoms), j = 1, tpl%type(n)%nmol)]

			end do ! for n

			! Create title
			ndx%group(next)%title = "Group "//str(next-1)

			! Count number of valid elements
			ndx%group(next)%natoms = count(mask)

			! Allocate data
			if (allocated(ndx%group(next)%loc)) deallocate(ndx%group(next)%loc, STAT=stat)
			allocate(ndx%group(next)%loc(ndx%group(next)%natoms), STAT=stat)

			! Use 'System' group as template for PACK
			ndx%group(next)%loc = pack(ndx%group(1)%loc(:), MASK=mask)

			! Increment global counter
			next = next+1

		end do

		! Clean-up
		if (allocated(mask)) deallocate(mask, STAT=stat)

		return
	end subroutine tpl2ndx

end program main
