module input
	use const
	implicit none 

	! gensi
	double precision, parameter :: nuc_z = 1.d0
	double precision, parameter :: nuc_a = 1.d0
	integer, parameter :: k(3) = (/ 3, 2, 2 /)
	double precision, save :: x(3), r(3)

	! plot
	double precision, parameter :: plot_x = 20.d0 !-x ~ x
	integer, parameter :: plot_n = 200 !**2 ko
	double precision, parameter :: plot_dx = 2.d0*plot_x/plot_n
	double precision, parameter :: total_n = 8.d3 !dai tai no kosu

contains





function psi(k, r) result(f)
	integer, intent(in) :: k(3)
	integer :: n, l, m
	double precision, intent(in) :: r(3)
	double precision :: rho, theta, phi
	double precision :: f

	! k
	n = k(1)
	l = k(2)
	m = k(3)

	! r
	rho = r(1)
	theta = r(2)
	phi = r(3)

	! n = 1
	if(n == 1) then
		! l = 0
		if(l == 0) then
			! m = 0
			if(m == 0) then
				f = 1.d0/(mt_pi)**0.5d0 &
					*(nuc_z/nuc_a)**1.5d0 &
					*exp(-rho)
			else 
				f = 0.d0
			endif
		else 
			f = 0.d0
		endif

	! n = 2
	else if(n == 2) then
		! l = 0
		if(l == 0) then
			! m = 0
			if(m == 0) then
				f = 1.d0/4.d0/(2.d0*mt_pi)**0.5d0 &
					*(nuc_z/nuc_a)**1.5d0 &
					*(2.d0-rho)*exp(-rho/2.d0)
			else 
				f = 0.d0
			endif
		! l = 1
		else if(l == 1) then
			! m = 0
			if(m == 0) then
				f = 1.d0/4.d0/(2.d0*mt_pi)**0.5d0 &
					*(nuc_z/nuc_a)**1.5d0 &
					*rho*exp(-rho/2.d0) &
					*cos(theta)
			! m = 1
			else if(m == 1) then
				f = 1.d0/4.d0/(2.d0*mt_pi)**0.5d0 &
					*(nuc_z/nuc_a)**1.5d0 &
					*rho*exp(-rho/2.d0) &
					*sin(theta)*cos(phi)
			else
				f = 0.d0
			endif
		else 
			f = 0.d0
		endif

	! n = 3
	else if(n == 3) then
		! l = 0
		if(l == 0) then
			! m = 0
			if(m == 0) then
				f = 1.d0/81.d0/(3.d0*mt_pi)**0.5d0 &
					*(nuc_z/nuc_a)**1.5d0 &
					*(27.d0 -18.d0*rho +2.d0*rho**2.d0)*exp(-rho/3.d0)
			else
				f = 0.d0
			endif
		! l = 1
		else if(l == 1) then
			! m = 0
			if(m == 0) then
				f = 2.d0**0.5d0/81.d0/(mt_pi)**0.5d0 &
					*(nuc_z/nuc_a)**1.5d0 &
					*rho*(6.d0 -rho)*exp(-rho/3.d0) &
					*cos(theta)
			! m = 1
			else if(m == 1) then
				f = 2.d0**0.5d0/81.d0/(mt_pi)**0.5d0 &
					*(nuc_z/nuc_a)**1.5d0 &
					*rho*(6.d0 -rho)*exp(-rho/3.d0) &
					*sin(theta)*cos(phi)
			else
				f = 0.d0
			endif
		! l = 2
		else if(l == 2) then
			! m = 0
			if(m == 0) then
				f = 1.d0/81.d0/(6.d0*mt_pi)**0.5d0 &
					*(nuc_z/nuc_a)**1.5d0 &
					*rho**2.d0*exp(-rho/3.d0) &
					*(3.d0*cos(theta)**2.d0 -1.d0)
			! m = 1
			else if(m == 1) then
				f = 2.d0**0.5d0/81.d0/(mt_pi)**0.5d0 &
					*(nuc_z/nuc_a)**1.5d0 &
					*rho**2.d0*exp(-rho/3.d0) &
					*sin(theta)*cos(theta)*cos(phi)
			! m = 2
			else if(m == 2) then
				f = 1.d0/81.d0/(2.d0*mt_pi)**0.5d0 &
					*(nuc_z/nuc_a)**1.5d0 &
					*rho**2.d0*exp(-rho/3.d0) &
					*sin(theta)**2.d0*cos(2.d0*phi)
			else
				f = 0.d0
			endif
		else
			f = 0.d0
		endif
	else 
		f = 0.d0
	endif

end function psi





subroutine xyz_to_polar(x, r)
	double precision, intent(in) :: x(3)
	double precision, intent(out) :: r(3)

	r(1) = (x(1)**2.d0 +x(2)**2.d0 +x(3)**2.d0)**0.5d0 !rho
	r(2) = acos(x(3)/r(1)) !theta
	r(3) = atan(x(2)/x(1)) !phi

	!ookii sugiru toki no taisaku

end subroutine xyz_to_polar





subroutine polar_to_xyz(r, x)
	double precision, intent(in) :: r(3)
	double precision, intent(out) :: x(3)
	
	x(1) = r(1)*sin(r(2))*cos(r(3))
	x(2) = r(1)*sin(r(2))*sin(r(3))
	x(3) = r(1)*cos(r(2))

end subroutine polar_to_xyz





subroutine random_f(f, random)
	double precision, intent(in) :: f
	double precision, intent(out) :: random
	double precision :: tmp

	call random_number(tmp)

	if(tmp < f) then
		random = 1.d0
	else 
		random = 0.d0
	endif
	
end subroutine random_f

end module input










program main
	use const
	use input
	implicit none 	

	integer :: ob
	character(3) :: ch3
	double precision :: sum, p, rdm(0:3), tem(3)
	integer :: i1, i2, i3, j1, j2, j3
	double precision :: box_n, box_dx
	integer :: box_s, dots, box_dots

	!open
	ob = 100*k(1) +10*k(2) +k(3)
	open(ob, file = 'ob_'//ch3(ob)//'.d')

	! sum
	sum = 0.d0
	do i1 = 1, plot_n
	do i2 = 1, plot_n
	do i3 = 1, plot_n
		x(1) = -plot_x -0.5d0*plot_dx +dble(i1)*plot_dx
		x(2) = -plot_x -0.5d0*plot_dx +dble(i2)*plot_dx
		x(3) = -plot_x -0.5d0*plot_dx +dble(i3)*plot_dx
		call xyz_to_polar(x(:), r(:))
		sum = sum &
				+psi(k(:), r(:))*psi(k(:), r(:)) &
				*plot_dx*plot_dx*plot_dx
	enddo
	enddo
	enddo

	! n
	box_n = total_n/sum





	! plot
	dots = 0
	do i1 = 1, plot_n
	do i2 = 1, plot_n
	do i3 = 1, plot_n

		! x, r
		x(1) = -plot_x -0.5d0*plot_dx +dble(i1)*plot_dx
		x(2) = -plot_x -0.5d0*plot_dx +dble(i2)*plot_dx
		x(3) = -plot_x -0.5d0*plot_dx +dble(i3)*plot_dx
		call xyz_to_polar(x(:), r(:))

		! box
		p = psi(k(:), r(:))*psi(k(:), r(:)) &
				*plot_dx*plot_dx*plot_dx

		call random_f(p*box_n -int(p*box_n), rdm(0))
		box_s = int(p*box_n) +int(rdm(0))

		do j1 = 1, box_s
			call random_number(rdm(1))
			call random_number(rdm(2))
			call random_number(rdm(3))
			tem(1) = x(1) -0.5d0*plot_dx +rdm(1)*plot_dx
			tem(2) = x(2) -0.5d0*plot_dx +rdm(2)*plot_dx
			tem(3) = x(3) -0.5d0*plot_dx +rdm(3)*plot_dx
			write(ob, *) tem(1), tem(2), tem(3)
			dots = dots +1
		enddo
	enddo
	enddo
	enddo

	! close
	close(ob)
	write(*, *) ob, 'obital, number of dots', dots, int(total_n)

end program main