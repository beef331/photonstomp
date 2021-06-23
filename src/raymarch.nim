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
camera.pos = vec3(ChunkEdgeSize / 2, 20, ChunkEdgeSize / 2)
camera.matrix = lookAt(camera.pos, vec3(ChunkEdgeSize / 2), vec3(0, 1, 0))
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
  lightPos = vec3(10, 0, 0)
  chunkData: Chunk
var i = 0
for blck in chunkData.mitems:
  let
    x = i mod ChunkEdgeSize
    y = i div (ChunkEdgeSize * ChunkEdgeSize)
    z = i div ChunkEdgeSize
  if y == 0:
    blck = dirt
  if i mod 3 == 0:
    blck = air

  inc i

let
  cameraUbo = shader.genUbo[:Camera]("Camera")
  lightUbo = shader.genUbo[:Vec3]("Light")
  blockSsbo = shader.genSsbo[:Chunk](3)

camera.copyTo cameraUbo
lightPos.copyTo lightUbo
chunkData.copyTo blockSsbo

shader.setUniform("chunkSize", ChunkEdgeSize)
while app.isRunning:
  let startTime = getMonoTime()
  app.poll
  app.draw
  time += (getMonoTime() - startTime).inMicroseconds.float / 1_000_000f

  camera.pos.y = 20 + sin(time) * 10
  camera.matrix = lookAt(camera.pos, vec3(ChunkEdgeSize / 2), vec3(0, 0, -1))

  camera.copyTo cameraUbo

  shader.setUniform("time", time)

glDeleteContext(app.context)
app.window.destroyWindow

