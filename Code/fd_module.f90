!file fd_module.f90 

module fd_module
use utility, only : fp
use setup_module, only: Nx

implicit none

contains

function dt_diffusion(C, dx, k) result(timestep)
  implicit none
  real(fp), intent(in) :: C, dx, k
  real(fp) :: timestep  
  timestep = C*((dx**2.0)/(2.0*k))
end function dt_diffusion

function dt_advection(C, a, dx) result(timestep)
  implicit none
  real(fp), intent(in) :: C, a, dx
  real(fp) :: timestep  
  timestep = C*(dx/ABS(a))
end function dt_advection

! first upwind routine
subroutine first_upwind() !eqn 30
  implicit none
  real(fp) :: u_prev(0: Nx+1), u(0:Nx+1)
  real(fp) :: a, dt, dx
  integer :: i
  u(0) = u(Nx)
  u(Nx+1) = u(1)
  u_prev(0:Nx+1) = u(0:Nx+1)
  !do i = 1, Nx
  do i = 0, Nx+1
    u(i) = u_prev(i) - (a*dt/dx)*(u_prev(i) - u_prev(i-1))
  enddo   
end subroutine first_upwind

! second center routine
subroutine second_center() !eqn 32
  implicit none
  real(fp) :: u_prev(0: Nx+1), u(0:Nx+1)
  real(fp) :: a, dt, dx
  integer :: i
  u(0) = u(Nx)
  u(Nx+1) = u(1)
  u_prev(0:Nx+1) = u(0:Nx+1)
  do i = 0, Nx+1
    u(i) = u_prev(i) - ((a*dt)/(2.0*dx))*(u_prev(i+1)-u_prev(i-1))
  enddo  
end subroutine second_center
  
! lax wendroff routine 
subroutine lax_wendroff() !eqn 35
  implicit none
  real(fp) :: u_prev(0: Nx+1), u(0:Nx+1)
  real(fp) :: a, dt, dx
  integer :: i
  u(0) = u(Nx)
  u(Nx+1) = u(1)
  u_prev(0:Nx+1) = u(0:Nx+1)
  do i = 0, Nx+1
    u(i) = u_prev(i) - (((a*dt)/(2.0*dx))*(u_prev(i+1) - u_prev(i-1))) & 
    & + (1.0/2.0)*(((a*dt)/dx)**2)*(u_prev(i+1) - 2.0*u_prev(i) + u_prev(i-1))
  enddo
end subroutine lax_wendroff

end module fd_module