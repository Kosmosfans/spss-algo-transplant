!> @brief demo program for K-Means clustering using the kmeans_core module, reading from a file.
!> @author LC
!> @date 2025-10-23
program kmeans_main_from_file
  use kmeans_core, only: kmeans
  use read_file_module, only: read_numeric_csv
  implicit none

  ! config
  integer :: n_clusters = 4
  integer :: max_iter = 200
  character(len=255) :: file_path = './data/clustering_data.csv'

  ! define input dataset
  integer :: n_cases
  integer :: n_vars
  real, allocatable :: data(:,:)

  ! define outputs
  real, allocatable :: final_centers(:,:)
  integer, allocatable :: final_assignments(:)
  integer, allocatable :: final_counts(:)

  integer :: i
  character(len=100) :: fmt
  character(len=10) :: f_n_vars

  data = read_numeric_csv(trim(file_path))
  n_cases = size(data, 1)
  n_vars = size(data, 2)
  
  ! Allocate arrays for outputs
  allocate(final_assignments(n_cases))
  allocate(final_centers(n_clusters, n_vars))
  allocate(final_counts(n_clusters))

  ! call K-Means clustering
  call kmeans(data, n_clusters, max_iter, final_centers, final_assignments, final_counts)

  ! print results
  print *, "========================================="
  print *, "K-Means Clustering Results"
  print *, "========================================="

  print *
  print *, "Final Cluster Centers:"
  write(f_n_vars, '(I0)') n_vars
  fmt = '(A, I2, A, ' // trim(f_n_vars) // 'F8.4)'
  do i = 1, n_clusters
    print fmt, "Cluster ", i, ": ", final_centers(i, :)
  end do

  print *
  print *, "Number of cases in each cluster:"
  do i = 1, n_clusters
    print '("Cluster ", I2, ": ", I0, " cases")', i, final_counts(i)
  end do

end program kmeans_main_from_file
