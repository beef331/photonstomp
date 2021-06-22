import opengl, vmath
import cameras
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
  

layout (std140) uniform Light
{
  vec3 pos;
} light;

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
  result.rayDir = normalize(vec3(coord - vec2(0.5), 0.7));
  vec3 pos = camera.pos_dist.xyz;
  for(int i = 0; i < camera.pos_dist.w; i++){
    vec3 normal = normalize(pos);
    if(length(pos) <= 2.0001){
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
  vec3 lightPos = light.pos.xyz;
  RayResult res = rayMarch(uv);
  if(res.hit > 0){
    vec3 color = (res.normal + 1) / 2;
    float lVal = dot(res.normal, normalize(lightPos)) * 0.5 + 0.5;
    float rim = (dot(res.normal, vec3(0, 0, 1)) * 0.5 + 0.5);
    FragColor = vec4(color * (round(lVal / 0.2) * 0.2) + (rim * color), 1);
  }else{
    FragColor = vec4(0);
  }
} 
"""
type
  ShaderKind* = enum
    Vertex, Fragment, Compute
  Ubo*[T] = distinct Gluint
  Ssbo*[T] = distinct Gluint

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


proc getUbo*[T](shader: Gluint, uniform: string): Ubo[T] =
  let index = glGetUniformBlockIndex(shader, uniform)
  glGenBuffers(1, Gluint(result).unsafeaddr)
  glBindBufferbase(GlUniformBuffer, index, Gluint(result))

proc `value=`*[T](ubo: Ubo[T], val: T) =
  glNamedBufferData(ubo.Gluint, sizeof(T), val.unsafeAddr, GlDynamicDraw)

proc getSsbo*[T](shader: Gluint, uniform: string): Ssbo[T] =
  let index = glGetUniformBlockIndex(shader, uniform)
  glGenBuffers(1, Gluint(result).unsafeaddr)
  glBindBufferbase(GlShaderStorageBuffer, index, Gluint(result))

proc setUniform*(shader: Gluint, uniform: string, value: float32) =
  let loc = glGetUniformLocation(shader, uniform)
  if loc != -1:
    glUniform1f(loc, value.GlFloat)
