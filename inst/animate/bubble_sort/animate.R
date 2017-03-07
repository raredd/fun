## animate bubble sort
library('animation')

saveHTML({
  library('fun')
  set.seed(1)
  bubble_sort(runif(100), TRUE)
},
  interval = 0.1, img.name = 'bubble_sort', imgdir = './fig',
  htmlfile = './bubble_sort.html', autobrowse = FALSE,
  title = 'bubble sort algorithm',
  description = 'bubble sort of 100 random uniform numbers'
)
