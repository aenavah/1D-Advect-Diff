! File: advect_diff.f90

program AdvectionDiffusion
  use utility, only : fp, pi
  use setup_module, only : initialize, filename, x_a, x_b, C, k, dx, dt, error, tolerance, Nx, prob_type, a
  use fd_module, only : dt_advection, dt_diffusion, first_upwind, second_center, lax_wendroff !advection_gc, 
  use error_module, only: error_comp
  use output_module, only: write_data, write_data_advection_x, write_data_advection_sol
  implicit none

  
  !define variables
  integer :: i , timestep, pass
  real(fp), allocatable :: x(:), u(:), u_prev(:)
  real(fp) :: M, tmax, time_passed
  character(len = 3) :: C_string 
  character(len = 3) :: Nx_string

  !initialize variables
  call initialize()
  print *, "Running ..." 

  !For file naming
  write(Nx_string, '(I3)') Nx
  Nx_string = trim(adjustl(Nx_string))

  write(C_string, '(F3.1)') C  
  C_string = trim(adjustl(C_string))

!Diffusion------------------------------------------------------
  if (prob_type == 1) then
    print * , "Diffusion ..."
    !allocate arrays
    call allocate()
    !compute dx 
    dx = (x_b - x_a)/Nx
    !fill x 
    do i = 1, Nx
     x(i) = x_a + (i - (1.0/2.0))*dx
    end do 
    dt = dt_diffusion(C, dx, k)
    pass = 0
    call discretization_diff()
    pass = 1
    call discretization_diff()
    ! deallocate arrays 
    call deallocate()
  endif

!Advection------------------------------------------------------
  if (prob_type == 0) then
  print * , "Advection ..."
  call allocate()
  dx = (x_b - x_a)/Nx
  dt = dt_advection(C, a, dx) 
  tmax = 1.0/a
  M = 1.0/dt ! :/ y
  do i = 0, Nx+1
    x(i) = x_a + (i - (1.0/2.0))*dx
  end do 

  
  ! write x positions in top of each data file ------------
  filename = "discrete_upwind_N=" // Nx_string // "_C=" // C_string // ".dat"
  call write_data_advection_x(filename, x)
  filename = "discrete_secondcenter_N=" // Nx_string // "_C=" // C_string // ".dat"
  call write_data_advection_x(filename, x)
  filename = "discrete_laxwendroff_N=" // Nx_string // "_C=" // C_string // ".dat"
  call write_data_advection_x(filename, x)
  filename = "cont_upwind_N=" // Nx_string // "_C=" // C_string // ".dat"
  call write_data_advection_x(filename, x)
  filename = "cont_secondcenter_N=" // Nx_string // "_C=" // C_string // ".dat"
  call write_data_advection_x(filename, x)
  filename = "cont_laxwendroff_N=" // Nx_string // "_C=" // C_string // ".dat"
  call write_data_advection_x(filename, x)


  
  ! initialize discrete data --------------
  time_passed = 0.0
  call advection_initialize_discrete()
  filename = "discrete_upwind_N=" // Nx_string // "_C=" // C_string // ".dat"
  call write_data_advection_sol(filename, u(1: Nx))       
  do while (time_passed < tmax)
    call first_upwind()
    filename = "discrete_upwind_N=" // Nx_string // "_C=" // C_string // ".dat"
    call write_data_advection_sol(filename, u(1: Nx))    
    time_passed = time_passed + dt
  end do   
  
  ! initialize discrete data --------------
  time_passed = 0.0
  call advection_initialize_discrete()
  filename = "discrete_secondcenter_N=" // Nx_string // "_C=" // C_string // ".dat"
  call write_data_advection_sol(filename, u(1: Nx))       
  do while (time_passed < tmax)
    call second_center()
    filename = "discrete_secondcenter_N=" // Nx_string // "_C=" // C_string // ".dat"
    call write_data_advection_sol(filename, u(1: Nx))       
    time_passed = time_passed + dt
  end do 
  
  ! initialize discrete data --------------
  time_passed = 0.0
  call advection_initialize_discrete()
  filename = "discrete_laxwendroff_N=" // Nx_string // "_C=" // C_string // ".dat"
  call write_data_advection_sol(filename, u(1: Nx))       
  do while (time_passed < tmax)
    call lax_wendroff()
    filename = "discrete_laxwendroff_N=" // Nx_string // "_C=" // C_string // ".dat"
    call write_data_advection_sol(filename, u(1: Nx))       
    time_passed = time_passed + dt
  end do

  ! initialize continuous data --------------
  time_passed = 0.0
  call advection_initialize_continuous()
  filename = "cont_upwind_N=" // Nx_string // "_C=" // C_string // ".dat"
  call write_data_advection_sol(filename, u(1: Nx))       
  do while(time_passed < tmax)
    call first_upwind()
    filename = "cont_upwind_N=" // Nx_string // "_C=" // C_string // ".dat"
    call write_data_advection_sol(filename, u(1: Nx))       
    time_passed = time_passed + dt
  enddo

  ! initialize continuous data --------------
  time_passed = 0.0
  call advection_initialize_continuous()
  filename = "cont_secondcenter_N=" // Nx_string // "_C=" // C_string // ".dat"
  call write_data_advection_sol(filename, u(1: Nx))  
  do while(time_passed < tmax)
    call second_center()
    filename = "cont_secondcenter_N=" // Nx_string // "_C=" // C_string // ".dat"
    call write_data_advection_sol(filename, u(1: Nx))  
    time_passed = time_passed + dt     
  enddo 
  
  ! initialize continuous data --------------
  time_passed = 0.0
  call advection_initialize_continuous()
  filename = "cont_laxwendroff_N=" // Nx_string // "_C=" // C_string // ".dat"
  call write_data_advection_sol(filename, u(1: Nx))       
  do while(time_passed < tmax)
    call lax_wendroff() 
    filename = "cont_laxwendroff_N=" // Nx_string // "_C=" // C_string // ".dat"
    call write_data_advection_sol(filename, u(1: Nx))       
    time_passed = time_passed + dt
  enddo  
  call deallocate()
 endif

!Contains--------------------------------------------------------------------
  contains 
  subroutine allocate()
    implicit none
    allocate(x(0:Nx+1))
    allocate(u(0:Nx+1))
    allocate(u_prev(0:Nx+1))
  end subroutine allocate 

  subroutine deallocate()
    implicit none
    deallocate(x)
    deallocate(u)
    deallocate(u_prev)
  end subroutine deallocate

!diffusion problem main -------------
  subroutine discretization_diff()
    implicit none     
    real(fp) :: epsilon, lower_bound, upper_bound, timestep_ratio

    !initialize u 
    u(0:Nx) = 0.0
    u(Nx+1) = 100.0
    timestep = 0 !first iteration is run 
    if (pass == 1) then 
      filename = "diff_0_N=" // Nx_string // ".dat"
      print *, "0% timestep found"
      call write_data(filename, u, x, x_a, x_b)
    endif
    error = 1

    !continue computing until tolerance is met 
    do while (error > tolerance)
      u_prev(0:Nx+1) = u(0:Nx+1)

      !compute next iteration of u
      do i = 1, Nx
        u(i) = u_prev(i) + k * (dt/(dx**2.0)) * (u_prev(i+1) - 2.0 * u_prev(i) + u_prev(i-1))
      end do 
      timestep = timestep + 1

      ! write data
      if (pass == 1) then
        timestep_ratio = timestep/M 
        lower_bound = timestep_ratio - epsilon
        upper_bound = timestep_ratio + epsilon
        epsilon = .00015

        if (lower_bound < .200 .and. .200 < upper_bound) then
          print *, "20% timestep found "
          filename = "diff_20_N=" // Nx_string // ".dat"
          !filename = "diff_20.dat"
          call write_data(filename, u, x, x_a, x_b)
        endif 

        if (lower_bound < 0.500 .and. 0.500 < upper_bound) then
          print *, "50% timestep found "
          filename = "diff_50_N=" // Nx_string // ".dat"
          call write_data(filename, u, x, x_a, x_b)
        endif 

        if (lower_bound < 0.800 .and. 0.800 < upper_bound) then
          print *, "80% timestep found "
          filename = "diff_80_N=" // Nx_string // ".dat"
          call write_data(filename, u, x, x_a, x_b)
        endif  

        if (timestep_ratio == 1.0) then
          print *, "100% timestep found "
          filename = "diff_100_N=" // Nx_string // ".dat"
          call write_data(filename, u, x, x_a, x_b)
        endif                                
      endif 

      !check error 
      error = error_comp(Nx, dt, u, u_prev)
    end do 

  if (pass == 0) then  
    M = timestep
  endif
  print *, "Total timesteps: ", M
  print *, "Total time: ", M *dt
  end subroutine discretization_diff

!advection problem main -------------

  ! initialize x and u for continuous
  subroutine advection_initialize_continuous
    do i = 0, Nx + 1
      x(i) = x_a + (i - (1.0/2.0))*dx !move ghost cells 
      u(i) = sin(2.0*pi*x(i))
     end do
  end subroutine advection_initialize_continuous

  !intialize u for discrete
  subroutine advection_initialize_discrete()
    do i = 1, Nx
      if (0.0 <= x(i) .and. x(i) <= 1.0/3.0) then 
        u(i) = -1
      end if
      if ((1.0)/(3.0) <= x(i) .and. x(i) <= (2.0)/(3.0)) then 
        u(i) = 1
      end if
      if ((2.0)/(3.0) <= x(i) .and. x(i) <= 1.0) then 
        u(i) = -1
      end if
    end do
  end subroutine advection_initialize_discrete



end program 

