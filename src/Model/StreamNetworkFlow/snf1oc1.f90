module SnfOcModule

  use BaseDisModule, only: DisBaseType
  use KindModule, only: DP, I4B
  use ConstantsModule, only: LENMODELNAME
  use OutputControlModule, only: OutputControlType
  use OutputControlDataModule, only: OutputControlDataType, ocd_cr

  implicit none
  private
  public SnfOcType, oc_cr

  !> @ brief Output control for GWF
  !!
  !!  Concrete implementation of OutputControlType for the
  !!  GWF Model
  !<
  type, extends(OutputControlType) :: SnfOcType
  contains
    procedure :: oc_ar
  end type SnfOcType

contains

  !> @ brief Create SnfOcType
  !!
  !!  Create by allocating a new SnfOcType object and initializing
  !!  member variables.
  !!
  !<
  subroutine oc_cr(ocobj, name_model, inunit, iout)
    ! -- dummy
    type(SnfOcType), pointer :: ocobj !< SnfOcType object
    character(len=*), intent(in) :: name_model !< name of the model
    integer(I4B), intent(in) :: inunit !< unit number for input
    integer(I4B), intent(in) :: iout !< unit number for output
    !
    ! -- Create the object
    allocate (ocobj)
    !
    ! -- Allocate scalars
    call ocobj%allocate_scalars(name_model)
    !
    ! -- Save unit numbers
    ocobj%inunit = inunit
    ocobj%iout = iout
    !
    ! -- Initialize block parser
    call ocobj%parser%Initialize(inunit, iout)
    !
    ! -- Return
    return
  end subroutine oc_cr

  !> @ brief Allocate and read SnfOcType
  !!
  !!  Setup head and budget as output control variables.
  !!
  !<
  subroutine oc_ar(this, stage, dis, dnodata)
    ! -- dummy
    class(SnfOcType) :: this !< SnfOcType object
    real(DP), dimension(:), pointer, contiguous, intent(in) :: stage !< model stage
    class(DisBaseType), pointer, intent(in) :: dis !< model discretization package
    real(DP), intent(in) :: dnodata !< no data value
    ! -- local
    integer(I4B) :: i, nocdobj, inodata
    type(OutputControlDataType), pointer :: ocdobjptr
    real(DP), dimension(:), pointer, contiguous :: nullvec => null()
    !
    ! -- Initialize variables
    inodata = 0
    nocdobj = 1
    allocate (this%ocdobj(nocdobj))
    do i = 1, nocdobj
      call ocd_cr(ocdobjptr)
      select case (i)
      case (1)
        call ocdobjptr%init_dbl('BUDGET', nullvec, dis, 'PRINT LAST ', &
                                'COLUMNS 10 WIDTH 11 DIGITS 4 GENERAL ', &
                                this%iout, dnodata)
      !case (2)
        ! if stage is ever a dependent variable, we can activate following line
        !call ocdobjptr%init_dbl('STAGE', head, dis, 'PRINT LAST ', &
        !                        'COLUMNS 10 WIDTH 11 DIGITS 4 GENERAL ', &
        !                        this%iout, dnodata)
      end select
      this%ocdobj(i) = ocdobjptr
      deallocate (ocdobjptr)
    end do
    !
    ! -- Read options or set defaults if this package not on
    if (this%inunit > 0) then
      call this%read_options()
    end if
    !
    ! -- Return
    return
  end subroutine oc_ar

end module SnfOcModule