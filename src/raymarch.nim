import sdl2/sdl, opengl, vmath
import raymarch/[shaders, cameras, blocks]
import std/[monotimes, times]

const
  WindowFlags = WindowOpenGl

type App = object
  window: Window
  context: GLContext
  isRunning: bool
  rect: Gluint

var camera = Camera(size: vec4(1280, 720, 0, 0), distance: 1000f)
camera.pos = vec3(ChunkEdgeSize.float + 2, 5, ChunkEdgeSize / 2)
camera.matrix = rotateY(90f.toRadians) * rotateX(-45f.toRadians)

proc init: App =
  if init(INIT_VIDEO) == 0:
    result.isRunning = true
    discard glSetAttribute(GL_CONTEXT_MAJOR_VERSION, 4)
    discard glSetAttribute(GL_CONTEXT_MINOR_VERSION, 3)

    result.window = createWindow("Test", WindowPosUndefined, WindowPosUndefined, camera.size.x.int,
        camera.size.y.int, WindowFlags)
    result.context = glCreateContext(result.window)
    loadExtensions()
    glClearColor(0.0, 0.0, 0.0, 1)


var mouseY = 0

proc poll(app: var App) =
  var e: Event
  mouseY = 0
  while pollEvent(addr(e)) != 0:
    # Quit requested
    if e.kind == Quit:
      app.isRunning = false
    # Key pressed
    elif e.kind == KeyDown:
      if e.key.keysym.sym == K_Escape:
        app.isRunning = false
    elif e.kind == MouseWheel:
      mouseY = e.wheel.y

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

proc openGlDebug(source: GLenum,
    typ: GLenum,
    id: GLuint,
    severity: GLenum,
    length: GLsizei,
    message: ptr GLchar,
    userParam: pointer) {.stdcall.} =
  var str = newString(length)
  copyMem(str[0].addr, message, length)
  echo str


var app = init()
let shader = getDefaultShader()

bindRect(shader)
glUseProgram(shader)
glEnable(GlDebugOutput)
glDebugMessageCallback(openGlDebug, nil)
var
  time = 0f
  lightPos = vec3(10, 1, 0)
  chunkData: Chunk

var i = 0
for blk in chunkData.mitems:
  let
    x = i mod ChunkEdgeSize
    y = i div (ChunkEdgeSize * ChunkEdgeSize)
    z = i mod (ChunkEdgeSize * ChunkEdgeSize) div ChunkEdgeSize
  if y == 0:
    blk = dirt
  #if x == z:
  #  blk = stone
  inc i

let
  cameraUbo = shader.genUbo[:Camera, "Camera"]()
  lightUbo = shader.genUbo[:Vec3, "Light"]()
  blockSsbo = shader.genSsbo[:Chunk](3)

camera.copyTo cameraUbo
lightPos.copyTo lightUbo
chunkData.copyTo blockSsbo

shader.setUniform("chunkSize", ChunkEdgeSize)

var epsilon = 0f32

while app.isRunning:
  let startTime = getMonoTime()
  app.poll
  app.draw
  let dt = (getMonoTime() - startTime).inMicroseconds.float / 1_000_000f
  time += dt
  epsilon += dt * mouseY.float * 5

  lightPos.x = sin(time) * 10
  lightPos.copyTo lightUbo

  shader.setUniform("time", time)
  shader.setUniform("epsilon", epsilon)

glDeleteContext(app.context)
app.window.destroyWindow

