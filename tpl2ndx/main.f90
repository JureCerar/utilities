#ifndef _DEBUG
#define _DEBUG .false.
#endif
! Program name
#define _PROGRAM "cutdown"
! Program version
#ifndef _VERSION
#define _VERSION "v0.0.0"
#endif

program main
	use xslib
	use iso_fortran_env
	implicit none
	type(tpl_file)	:: tpl
	type(ndx_file)	:: ndx
	character*512	:: inFile="", outFile=""
	integer			:: i, j, n, next, offset, stat
	logical 		:: atoms=.true., mols=.true.

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

	! number of groups
	ndx%ngroups = 1
	if (mols) ndx%ngroups = ndx%ngroups+tpl%ntypes
	if (atoms) ndx%ngroups = ndx%ngroups+tpl%natoms

	if (allocated(ndx%group)) deallocate(ndx%group, STAT=stat)
	allocate(ndx%group(ndx%ngroups), STAT=stat)

	! Global group counter
	next = 1

	! ---------------------------
	! Entire system
	ndx%group(next)%title = "System"

	! Number of atoms in the system
	ndx%group(next)%natoms = sum(tpl%type(:)%natoms*tpl%type(:)%nmol)

	! Allocate memory
	if (allocated(ndx%group(next)%loc)) deallocate(ndx%group(next)%loc, STAT=stat)
	allocate(ndx%group(next)%loc(ndx%group(next)%natoms), STAT=stat)

	! Simple sequence of all numbers
	ndx%group(1)%loc(:) = [(i, i = 1, ndx%group(1)%natoms)]

	! Move global counter
	next = next+1

	! ------------------------------------------------------------------------
	! Molecules in the system

	do n = 1, tpl%ntypes
		! Adjust the title
		ndx%group(next)%title = "Molecule "//str(n)

		! Number of each molecules
		ndx%group(next)%natoms = tpl%type(n)%natoms*tpl%type(n)%nmol

		! Offset by previous molecules (atoms) in the box
		offset = sum(tpl%type(:n-1)%natoms*tpl%type(:n-1)%nmol)

		! Allocate memory
		if (allocated(ndx%group(next)%loc)) deallocate(ndx%group(next)%loc, STAT=stat)
		allocate(ndx%group(next)%loc(ndx%group(next)%natoms), STAT=stat)

		! Generate sequence of numbers
		ndx%group(next)%loc(:) = [(offset+i, i = 1, ndx%group(next)%natoms)]

		! Move global counter
		next = next+1

	end do

	! ------------------------------------------------------------------------
	! All different atoms in the system

	do n = 1, tpl%ntypes
		do i = 1, tpl%type(n)%natoms
			! Adjust the title
			ndx%group(next)%title = "Atom "//trim(tpl%type(n)%name(i))

			! Number of atoms
			ndx%group(next)%natoms = tpl%type(n)%nmol

			! Offset by previous molecule types
			offset = sum(tpl%type(:n-1)%natoms*tpl%type(:n-1)%nmol)

			! Allocate memory
			if (allocated(ndx%group(next)%loc)) deallocate(ndx%group(next)%loc, STAT=stat)
			allocate(ndx%group(next)%loc(ndx%group(next)%natoms), STAT=stat)

			! Generate sequence of numbers
			ndx%group(next)%loc(:) = [(offset+i+(j-1)*tpl%type(n)%natoms, j = 1, tpl%type(n)%nmol)]

			! Move global counter
			next = next+1

		end do
	end do

	! ------------------------------------------------------------------------
	! Write all groups to STDOUT just to check

	do n = 1, ndx%ngroups
		write (*,"(a)") "Group "//str(n)//" '"//ndx%group(n)%title//"' ("// &
		str(ndx%group(n)%natoms)//" atoms)"

	end do

	! --------------------------
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
		logical			:: exist

		! if (command_argument_count() == 0) go to 100

		next = 0
		do while (next <= command_argument_count())
			next = next+1
			call get_command_argument (next, arg, STATUS=stat)
				if (stat/=0 .or. verify(arg, " ")==0) exit

			select case (trim(arg))
			case ("-h","--help")
				write (*,*) _PROGRAM//" [options] [input]"
				write (*,*)	""
				write (*,*)	"DESCRIPTION:"
				write (*,*)	"  <I am a program>"
				write (*,*)	""
				write (*,*)	"OPTIONS:"
				write (*,*)	" -h, --help   -- Print this message."
				write (*,*) " -n   [i]     -- Parameter num. 1."
				write (*,*)	""
				write (*,*)	_PROGRAM//":	"//_VERSION//" - "//__DATE__//" "//__TIME__
				write (*,*)	"xslib:	"//trim(xslibINFO)
				if (_DEBUG) write (*,*) " --- DEBUG MODE --- "
				call exit (0)

			! case ("-n")
			! 	next = next+1
			! 	call get_command_argument (next, arg, STATUS=stat)
			! 	read (arg, *, IOSTAT=stat) np
			! 	if (stat /= 0 .or. np <= 0 .or. np > _MAXCOL) then
			! 		write (error_unit, *) "ERROR - Invalid argument: "//trim(arg)
			! 		call exit (1)
			! 	end if

			end select


		end do

		return
	end subroutine getOpts

end program main
