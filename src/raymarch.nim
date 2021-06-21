import sdl2/sdl, opengl, vmath
import raymarch/[shaders, cameras]
import std/times

const
  WindowFlags = WindowOpenGl or WindowShown

type App = object
  window: Window
  context: GLContext
  isRunning: bool
  rect: Gluint

var camera = Camera(size: vec2(640, 480), pos: vec3(0, 0, -10), distance: 100f)

proc init: App =
  if init(INIT_VIDEO) == 0:
    result.isRunning = true
    discard glSetAttribute(GL_CONTEXT_MAJOR_VERSION, 4)
    discard glSetAttribute(GL_CONTEXT_MINOR_VERSION, 0)

    result.window = createWindow("Test", WindowPosUndefined, WindowPosUndefined, camera.size.x.int,
        camera.size.y.int, WindowFlags)
    result.context = glCreateContext(result.window)
    loadExtensions()
    glClearColor(0.0, 0.0, 0.0, 1)

proc poll(app: var App) =
  var e: Event

  while pollEvent(addr(e)) != 0:

    # Quit requested
    if e.kind == Quit:
      app.isRunning = false
    # Key pressed
    elif e.kind == KeyDown:
      if e.key.keysym.sym == K_Escape:
        app.isRunning = false

proc draw(app: var App) =
  glClear(GL_ColorBufferBit)
  glDrawArrays(GL_TRIANGLES, 0, 3)
  glDrawArrays(GL_TRIANGLES, 2, 3)
  glSwapWindow(app.window)

proc bindRect(shader: Gluint) =
  let verts = [
    vec2(-1, -1),
    vec2(-1, 1),
    vec2(1, 1),
    vec2(1, -1),
    vec2(-1, -1)]
  let vbo: GLuint = 0
  glGenBuffers(1, vbo.unsafeaddr)
  glBindBuffer(GlArrayBuffer, vbo)
  glBufferData(GlArrayBuffer, verts.sizeof, verts.unsafeaddr, GlStaticDraw)

  let vao: Gluint = 0
  glGenVertexArrays(1, vao.unsafeAddr)
  glBindVertexArray(vao)

  let posAttrib = glGetAttribLocation(shader, "aPos").Gluint
  glVertexAttribPointer(posAttrib, 2.GlInt, cGlFloat, GlFalse, 0.GlInt, nil)
  glEnableVertexAttribArray(posAttrib);




var app = init()
let shader = getDefaultShader()
bindRect(shader)
glUseProgram(shader)
shader.setUniformBuff("Camera", camera)
let start = cpuTime()
while app.isRunning:
  shader.setUniform("time", cpuTime() - start)
  app.poll
  app.draw

glDeleteContext(app.context)
app.window.destroyWindow