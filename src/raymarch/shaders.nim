import opengl
import std/[options, tables]
import cameras
export tables
const
  vertexShader = """
#version 330 core
layout (location = 0) in vec3 aPos;
  
void main()
{
    gl_Position = vec4(aPos, 1.0); // see how we directly give a vec3 to vec4's constructor
}
"""
  fragShader = """
#version 330 core
out vec4 FragColor;
  
in vec4 vertexColor; // the input variable from the vertex shader (same name and same type)

layout (std140) uniform Camera
{
  vec4 pos_dist;
  vec2 size;
} camera;

uniform float time;

struct RayResult{
  float hit;
  vec3 normal;
  vec3 rayDir;
};

RayResult rayMarch(vec2 coord){
  RayResult result;
  result.rayDir = normalize(vec3(coord - vec2(0.5), 1));
  vec3 pos = camera.pos_dist.xyz;
  for(int i = 0; i < camera.pos_dist.w; i++){
    vec3 normal = normalize(pos);
    if(length(pos) <= 2){
      result.normal = normal;
      result.hit = 1;
      break;
    }
    pos += abs(distance(pos, normal * 2)) * result.rayDir;
  }
  return result;
} 


void main()
{
  vec2 uv = gl_FragCoord.xy / camera.size;
  uv.y *= camera.size.y / camera.size.x;
  RayResult res = rayMarch(uv);
  if(res.hit > 0){
    vec3 color = (res.normal + 1) / 2;
    FragColor = vec4(color, 1);
  }else{
    FragColor = vec4(0);
  }
} 
"""
type
  ShaderKind* = enum
    Vertex, Fragment, Compute
  Ubo* = distinct Gluint

const KindLut = [
  Vertex: GlVertexShader,
  Fragment: GlFragmentShader,
  Compute: GlComputeShader
]


proc loadShader*(shader: string, kind: ShaderKind): Gluint =
  let
    shaderProg = allocCStringArray([shader])
    shaderId = glCreateShader(KindLut[kind])
  glShaderSource(shaderId, 1, shaderProg, nil)
  glCompileShader(shaderId)
  var success = 0.Glint
  glGetShaderiv(shaderId, GlCompileStatus, success.addr)

  if success == 0:
    var buff = newString(512)
    glGetShaderInfoLog(shaderId, 512, nil, buff[0].addr)
    echo buff
    return
  result = shaderId

  shaderProg.deallocCStringArray

proc getDefaultShader*(): GLuint =
  let
    vs = loadShader(vertexShader, Vertex)
    fs = loadShader(fragShader, Fragment)
  result = glCreateProgram()
  glAttachShader(result, vs)
  glAttachShader(result, fs)
  glLinkProgram(result)

  var success = 1.Glint

  glGetProgramIv(result, GlLinkStatus, success.addr)
  if success == 0:
    var msg = newString(512)
    glGetProgramInfoLog(result, 512, nil, msg[0].addr)
    echo msg
  glDeleteShader(vs)
  glDeleteShader(fs)


proc getUbo*(shader: Gluint, uniform: string): Ubo =
  let
    index = glGetUniformBlockIndex(shader, uniform)
    uboHandle: Gluint = 0
  glGenBuffers(1, uboHandle.unsafeaddr)
  glBindBuffer(GlUniformBuffer, uboHandle)
  glBindBufferbase(GlUniformBuffer, index, uboHandle)
  uboHandle.Ubo

proc `cameraBuffer=`*(ubo: Ubo, cam: Camera) =
  glNamedBufferData(ubo.Gluint, sizeof(cam), cam.unsafeAddr, GlDynamicDraw)

proc setUniform*(shader: Gluint, uniform: string, value: float32) =
  let loc = glGetUniformLocation(shader, uniform)
  if loc != -1:
    glUniform1f(loc, value.GlFloat)
