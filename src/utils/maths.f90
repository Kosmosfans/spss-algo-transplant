module maths
  implicit none
  private
  public :: weighted_random_sample

contains
  !--------------------------------------------------------------------
  ! Selects an index from the input weights array with probability
  ! proportional to the weights.
  ! Arguments:
  !   weights - array of weights (non-negative real numbers)
  ! Returns:
  !   idx     - selected index (1-based)
  !--------------------------------------------------------------------
  function weighted_random_sample(weights) result(idx)
    real, intent(in) :: weights(:)
    integer :: idx
    real :: total_weight, random_value, current_weight
    integer :: i

    ! 1. Calculate total weight
    total_weight = sum(weights)

    ! 2. Generate a random number
    call random_number(random_value) ! Generates a value in [0, 1)
    random_value = random_value * total_weight

    ! 3. Iterate and select
    current_weight = 0.0
    do i = 1, size(weights)
      current_weight = current_weight + weights(i)
      if (current_weight >= random_value) then
        idx = i
        return
      end if
    end do

    ! Fallback for floating point inaccuracies or if all weights are zero
    if (size(weights) > 0) then
        idx = size(weights)
    else
        idx = 0
    end if

  end function weighted_random_sample

end module maths
