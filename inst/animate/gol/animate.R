## animate gol examples
library('animation')

saveHTML({
  library('fun')
  source(system.file('source', 'gol_special.R', package = 'fun'))
  plot(play_gol(glider_gun, 200))
  },
  interval = 0.1, img.name = 'glider_gun', imgdir = './fig',
  htmlfile = './glider_gun.html', autobrowse = FALSE,
  title = 'game of life - glider gun',
  description = '200 generations of glider gun'
)
