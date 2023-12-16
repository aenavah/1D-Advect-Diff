! File: setup_module.f90 
module setup_module
use utility, only : fp, pi, maxStrLen 
use read_initFile_module, only: read_initFileInt, read_initFileReal!, read_initFileReal , read_initFileChar

implicit none
real(fp) :: a, x_a, x_b, dx, dt, sum, tolerance, k, C, error
real(fp), allocatable :: x(:), u(:), u_prev(:)
integer :: Nx, prob_type


character(len=maxStrLen) :: inFile, filename


contains
  subroutine initialize()
    implicit none
    tolerance = 1e-6
    !inFile = "diffusion_input.txt" !use advection_input.txt or diffusion_input.txt
    !inFile = "advection_input.txt" !use advection_input.txt or diffusion_input.txt
    inFile = "input.txt"
    !filename = "placeholder" !? idk why I have this variable
    prob_type = read_initFileInt(inFile,'type') !prob_type1 is advection
    k = read_initFileReal(inFile,'k')
    x_a = read_initFileReal(inFile,'x_a')
    x_b = read_initFileReal(inFile,'x_b')
    C = read_initFileReal(inFile,'C')
    a = read_initFileReal(inFile,'adv_vl')
    Nx = read_initFileInt(inFile,'Nx')
  end subroutine initialize
  
end module setup_module
