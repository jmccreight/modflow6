! This is the explicit solution module.

module ExplicitSolutionModule
  use KindModule,              only: I4B, DP   ! kluge note: DP needed for ttsoln
  use TimerModule,             only: code_timer
  use ConstantsModule,         only: LENMEMPATH, LENSOLUTIONNAME, MVALIDATE,   &
                                     MNORMAL, LINELENGTH, DZERO  ! kluge note: DZERO needed for ttsoln
  use MemoryHelperModule,      only: create_mem_path                                     
  use BaseModelModule,         only: BaseModelType,                            &
                                     AddBaseModelToList,                       &
                                     GetBaseModelFromList
  use BaseExchangeModule,      only: BaseExchangeType
  use BaseSolutionModule,      only: BaseSolutionType, AddBaseSolutionToList
  use ListModule,              only: ListType
  use ListsModule,             only: basesolutionlist
  use SimVariablesModule,      only: iout, isim_mode
  use BlockParserModule,       only: BlockParserType

  implicit none
  private

  public :: solution_explicit_create
  public :: ExplicitSolutionType
  
  type, extends(BaseSolutionType) :: ExplicitSolutionType
    character(len=LENMEMPATH)                        :: memoryPath            !< the path for storing solution variables in the memory manager
    type(ListType), pointer                          :: modellist             !< list of models in solution
    integer(I4B), pointer                            :: id                    !< solution number
    integer(I4B), pointer                            :: iu                    !< input file unit
    real(DP), pointer                                :: ttsoln                !< timer - total solution time
    integer(I4B), pointer                            :: icnvg => null()       !< convergence flag
    type(BlockParserType)                            :: parser                !< block parser object
  contains
    procedure :: sln_df
    procedure :: sln_ar
    procedure :: sln_calculate_delt
    procedure :: sln_ad
    procedure :: sln_ot
    procedure :: sln_ca
    procedure :: sln_fp
    procedure :: sln_da
    procedure :: add_model
    procedure :: add_exchange
    procedure :: get_models
    procedure :: get_exchanges
    procedure :: save

    procedure, private :: allocate_scalars
    
    ! Expose these for use through the BMI/XMI:
    procedure, public :: prepareSolve
    procedure, public :: solve
    procedure, public :: finalizeSolve
  
  end type ExplicitSolutionType

contains

!> @ brief Create a new solution
!!
!!  Create a new solution using the data in filename, assign this new 
!!  solution an id number and store the solution in the basesolutionlist.
!!  Also open the filename for later reading.
!!
!<
subroutine solution_explicit_create(filename, id)
    ! -- modules
    use SimVariablesModule, only: iout
    use InputOutputModule,  only: getunit, openfile
    ! -- dummy variables
    character(len=*),intent(in) :: filename    !< solution input file name
    integer(I4B),intent(in) :: id              !< solution id
    ! -- local variables
    integer(I4B) :: inunit
    type(ExplicitSolutionType), pointer :: solution => null()
    class(BaseSolutionType), pointer :: solbase => null()
    character(len=LENSOLUTIONNAME) :: solutionname
    !
    ! -- Create a new solution and add it to the basesolutionlist container
    allocate(solution)
    solbase => solution
    write(solutionname,'(a, i0)') 'SLN_', id
    !
    solution%name = solutionname
    solution%memoryPath = create_mem_path(solutionname)
    allocate(solution%modellist)
    !
    call solution%allocate_scalars()
    !
    call AddBaseSolutionToList(basesolutionlist, solbase)
    !
    solution%id = id
    !
    ! -- Open solution input file for reading later after problem size is known
    !    Check to see if the file is already opened, which can happen when
    !    running in single model mode
    inquire(file=filename, number=inunit)

    if(inunit < 0) inunit = getunit()
    solution%iu = inunit
    write(iout,'(/a,a/)') ' Creating explicit solution (EMS): ', solution%name
    call openfile(solution%iu, iout, filename, 'IMS')
    !
    ! -- Initialize block parser
    call solution%parser%Initialize(solution%iu, iout)
    !
    ! -- return
    return
  end subroutine solution_explicit_create

  !> @ brief Allocate scalars
  !!
  !!  Allocate scalars for a new solution.
  !!
  !<
  subroutine allocate_scalars(this)
    ! -- modules
    use MemoryManagerModule, only: mem_allocate
    ! -- dummy variables
    class(ExplicitSolutionType) :: this  !< ExplicitSolutionType instance
    !
    ! -- allocate scalars
    call mem_allocate(this%id, 'ID', this%memoryPath)
    call mem_allocate(this%iu, 'IU', this%memoryPath)
    call mem_allocate(this%ttsoln, 'TTSOLN', this%memoryPath)
    call mem_allocate(this%icnvg, 'ICNVG', this%memoryPath)
    !
    ! -- initialize
    this%id = 0
    this%iu = 0
    this%ttsoln = DZERO
    this%icnvg = 0
    !
    ! -- return
    return
  end subroutine allocate_scalars

  subroutine sln_df(this)
    class(ExplicitSolutionType) :: this
  end subroutine

  subroutine sln_ar(this)
    ! -- dummy variables
    class(ExplicitSolutionType) :: this  !< ExplicitSolutionType instance
    !
    ! -- close ems input file
    call this%parser%Clear()
    !
    ! -- return
    return
  end subroutine sln_ar

  subroutine sln_calculate_delt(this)
    class(ExplicitSolutionType) :: this  !< ExplicitSolutionType instance
  end subroutine sln_calculate_delt
  
  !> @ brief Advance solution
  !!
  !!  Advance solution.
  !!
  !<
  subroutine sln_ad(this)
    ! -- dummy variables
    class(ExplicitSolutionType) :: this  !< ExplicitSolutionType instance
    !
    ! -- reset convergence flag
    this%icnvg = 0

    return
  end subroutine sln_ad
  
  subroutine sln_ot(this)
    class(ExplicitSolutionType) :: this  !< ExplicitSolutionType instance
  end subroutine sln_ot

  subroutine sln_fp(this)
    class(ExplicitSolutionType) :: this  !< ExplicitSolutionType instance
  end subroutine sln_fp

  !> @ brief Deallocate solution
  !!
  !!  Deallocate a solution.
  !!
  !<
  subroutine sln_da(this)
    ! -- modules
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy variables
    class(ExplicitSolutionType) :: this  !< ExplicitSolutionType instance
    !
    ! -- lists
    call this%modellist%Clear()
    deallocate(this%modellist)
    !
    !
    ! -- Scalars
    call mem_deallocate(this%id)
    call mem_deallocate(this%iu)
    call mem_deallocate(this%ttsoln)
    call mem_deallocate(this%icnvg)
    !
    ! -- return
    return
  end subroutine sln_da

  !> @ brief Solve solution
  !!
  !!  Solve the models in this solution for kper and kstp.
  !!
  !<
  subroutine sln_ca(this, isgcnvg, isuppress_output)
    ! -- modules
    ! -- dummy variables
    class(ExplicitSolutionType) :: this             !< ExplicitSolutionType instance
    integer(I4B), intent(inout) :: isgcnvg          !< solution group convergence flag
    integer(I4B), intent(in) :: isuppress_output    !< flag for suppressing output
    ! -- local variables
    class(BaseModelType), pointer :: mp => null()
    character(len=LINELENGTH) :: line
    character(len=LINELENGTH) :: fmt
    integer(I4B) :: im
! ------------------------------------------------------------------------------
    
    ! advance the models and solution
    call this%prepareSolve()
    
    select case (isim_mode)
      case (MVALIDATE)
        line = 'mode="validation" -- Skipping assembly and solution.'
        fmt = "(/,1x,a,/)"
        do im = 1, this%modellist%Count()
          mp => GetBaseModelFromList(this%modellist, im)
          call mp%model_message(line, fmt=fmt)
        end do
      case(MNORMAL)
           
        ! solve the models
        call this%solve()
      
        ! finish up
        call this%finalizeSolve(isgcnvg, isuppress_output)
    end select
    !
    ! -- return
    return
    
  end subroutine sln_ca
       
  !> @ brief prepare to solve
  !!
  !!  Prepare for the system solve by advancing the simulation.
  !!
  !<
  subroutine prepareSolve(this)
    ! -- dummy variables
    class(ExplicitSolutionType) :: this  !< ExplicitSolutionType instance
    ! -- local variables
    integer(I4B) :: ic
    integer(I4B) :: im
    class(BaseModelType), pointer :: mp => null()    
    
    ! -- Model advance
    do im = 1, this%modellist%Count()
      mp => GetBaseModelFromList(this%modellist, im)
      call mp%model_ad()
    enddo

    ! advance solution
    call this%sln_ad()
    
  end subroutine prepareSolve
  
  !> @ brief Build and solve the simulation
  !!
  !! Builds and solves the system for this explicit solution. 
  !! It roughly consists of the following steps
  !! (1) ! kluge note: list steps
  !!
  !<
 subroutine solve(this)
    ! -- dummy variables
    class(ExplicitSolutionType) :: this     !< ExplicitSolutionType instance    
    ! -- local variables
    class(BaseModelType), pointer :: mp => null()
    integer(I4B) :: im    
    real(DP) :: ttsoln
    !
    ! -- particle solve
    call code_timer(0, ttsoln, this%ttsoln)   ! kluge, needed?
    do im = 1, this%modellist%Count()
      mp => GetBaseModelFromList(this%modellist, im)
      call mp%model_solve()
    end do
    call code_timer(1, ttsoln, this%ttsoln)   ! kluge, needed?
    !
    this%icnvg = 1
    
 end subroutine solve
  
  !> @ brief finalize a solution
  !!
  !!  Finalize the solution. Called after the outer iteration loop.
  !!
  !<
  subroutine finalizeSolve(this, isgcnvg, isuppress_output)
    ! -- dummy variables
    class(ExplicitSolutionType) :: this          !< ExplicitSolutionType instance
    integer(I4B), intent(inout) :: isgcnvg       !< solution group convergence flag
    integer(I4B), intent(in) :: isuppress_output !< flag for suppressing output
    ! -- local variables
    integer(I4B) :: ic, im
    class(BaseModelType), pointer :: mp => null()
    !
    ! -- Calculate flow for each model
    do im=1,this%modellist%Count()
      mp => GetBaseModelFromList(this%modellist, im)
      call mp%model_cq(this%icnvg, isuppress_output)
    enddo
    !
    ! -- Budget terms for each model
    do im=1,this%modellist%Count()
      mp => GetBaseModelFromList(this%modellist, im)
      call mp%model_bd(this%icnvg, isuppress_output)
    enddo
    !
  end subroutine finalizeSolve

  !> @ brief Save solution data to a file
  !!
  !!  ! kluge note: description
  !!
  !<
  subroutine save(this, filename)
    ! -- modules
    use InputOutputModule, only:getunit
    ! -- dummy variables
    class(ExplicitSolutionType) :: this       !< ExplicitSolutionType instance
    character(len=*), intent(in) :: filename  !< filename to save solution data
    ! -- local variables
    integer(I4B) :: inunit
    !
    inunit = getunit()
    open(unit=inunit,file=filename,status='unknown')
    write(inunit,*) 'The save routine currently writes nothing'  ! kluge
    close(inunit)
    !
    ! -- return
    return
  end subroutine save

  !> @ brief Add a model
  !!
  !!  Add a model to this%modellist.
  !!
  !<
  subroutine add_model(this, mp)
    ! -- dummy variables
    class(ExplicitSolutionType) :: this              !< ExplicitSolutionType instance
    class(BaseModelType), pointer, intent(in) :: mp  !< model instance
    ! -- local variables
    class(BaseModelType), pointer :: m => null()
    !
    ! -- add a model
    m => mp
    call AddBaseModelToList(this%modellist, m)
    !
    ! -- return
    return
  end subroutine add_model

  !> @brief Get a list of models
  !!
  !!  Returns a pointer to the list of models in this solution.
  !!
  !<
  function get_models(this) result(models)
    ! -- return variable
    type(ListType), pointer :: models    !< pointer to the model list
    ! -- dummy variables
    class(ExplicitSolutionType) :: this  !< ExplicitSolutionType instance

    models => this%modellist

  end function get_models

  subroutine add_exchange(this, exchange)
    class(ExplicitSolutionType) :: this
    class(BaseExchangeType), pointer, intent(in) :: exchange
  end subroutine add_exchange

  function get_exchanges(this) result(exchanges)
    class(ExplicitSolutionType) :: this
    type(ListType), pointer :: exchanges
  end function get_exchanges

end module ExplicitSolutionModule