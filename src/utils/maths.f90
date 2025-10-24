module maths
  implicit none
  private
  public :: weighted_random_sample

contains
  !--------------------------------------------------------------------
  ! Selects an index from the input weights array with probability
  ! proportional to the weights.
  !
  ! Arguments:
  !   weights - array of weights (non-negative real numbers)
  ! Returns:
  !   idx     - selected index (1-based)
  !--------------------------------------------------------------------
  function weighted_random_sample(weights) result(idx)
    real, intent(in) :: weights(:)
    integer :: idx
    real :: total_weight, r, cumulative_prob
    integer :: i

    ! Calculate total weight
    total_weight = sum(weights)

    ! Generate a random number from 0 to total_weight
    call random_number(r)
    r = r * total_weight

    ! Iterate until we find the index corresponding to the random number
    cumulative_prob = 0.0
    do i = 1, size(weights)
      cumulative_prob = cumulative_prob + weights(i)
      if (cumulative_prob >= r) then
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
