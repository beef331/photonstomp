import opengl
import std/[typetraits, tables]
const
  vertexShader = """
#version 430
layout (location = 0) in vec3 aPos;
  
void main()
{
    gl_Position = vec4(aPos, 1.0); // see how we directly give a vec3 to vec4's constructor
}
"""
  fragShader = """
#version 430
out vec4 fragColor;


layout (std140, binding = 1) uniform Camera
{
  vec4 pos_dist;
  vec4 size;
  mat4 matrix;
} camera;

layout (std140, binding = 2) uniform Light
{
  vec3 pos;
} light;

layout (std430, binding = 3) buffer blockData
{
  int ids[];
};

uniform float time;
uniform float epsilon;

uniform int chunkSize;

struct RayResult{
  int hit;
  vec3 normal;
  vec3 rayDir;
  vec3 pos;
};


vec2 boxIntersection( in vec3 ro, in vec3 rd, out vec3 oN ) 
{
    vec3 m = 1.0 / rd;
    vec3 n = m * ro;
    vec3 k = abs(m);
    vec3 t1 = -n - k;
    vec3 t2 = -n + k;

    float tN = max( max( t1.x, t1.y), t1.z);
    float tF = min( min( t2.x, t2.y), t2.z);

    oN = -sign(rd) * step(t1.yzx,t1.xyz) * step(t1.zxy,t1.xyz);

    return vec2( tN, tF );
}

RayResult rayMarch(vec2 coord){
  RayResult result;
  result.rayDir = normalize(mat3(camera.matrix) * vec3(coord, 1));
  float total = 0;
  for(int i = 0; i < camera.pos_dist.w; i++){
    vec3 pos = camera.pos_dist.xyz + total * result.rayDir;

    ivec3 floored = ivec3(pos);
    vec3 norm;
    vec2 hitDists = boxIntersection(camera.pos_dist.xyz - floored, result.rayDir, norm);
    total = hitDists.y + 0.00001;
    vec3 frontFace = (camera.pos_dist.xyz + (hitDists.x * 1.01) * result.rayDir);
    floored = ivec3(frontFace);

    if(frontFace.x >= 0 && frontFace.x < chunkSize &&
      frontFace.y >= 0 && frontFace.y < chunkSize &&
      frontFace.z >= 0 && frontFace.z < chunkSize){

      int index = floored.x + (floored.z * chunkSize) + (floored.y * chunkSize * chunkSize);
      int blockId = (ids[index / 2] >> (index % 2 * 0x10)) & 0xffff; // int32 -> int16
      if(blockId > 0){
        result.hit = blockId;
        result.normal = norm;
        result.pos = frontFace;
        break;
      }
    }
  }
  return result;
} 

void main()
{
  vec2 uv = vec2(gl_FragCoord.xy - 0.5 * camera.size.xy) / camera.size.y;
  vec3 lightPos = light.pos.xyz;

  RayResult res = rayMarch(uv);
  if(res.hit > 0){
    vec3 col = vec3(1);
    float light = dot(res.normal, normalize(lightPos)) * 0.5 + 0.5;
    fragColor = vec4(col * light, 1);
    fragColor = vec4((res.normal * 0.5 + 0.5), 1);
  }
  else{
    fragColor = vec4(0);
  }
} 
"""
type
  ShaderKind* = enum
    Vertex, Fragment, Compute
  Ubo*[T] = distinct Gluint
  Ssbo*[T] = distinct Gluint
  Shader* = distinct Gluint

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


const UboTable = {
  "Camera": 1.Gluint,
  "Light": 2.Gluint
  }.toTable

proc genUbo*[T; U: static[string]](shader: Gluint): Ubo[T] =
  glGenBuffers(1, result.Gluint.addr)
  glBindBuffer(GlUniformBuffer, result.Gluint)
  glBindBufferbase(GlUniformBuffer, UboTable[U], result.Gluint) # Apparently no way to go name -> Ubo bind location

proc copyTo*[T](val: T, ubo: Ubo[T]) =
  glBindBuffer(GlUniformBuffer, ubo.GLuint)
  glNamedBufferData(ubo.Gluint, sizeof(T), val.unsafeAddr, GlDynamicDraw)
  glBindBuffer(GlUniformBuffer, 0.Gluint)

proc genSsbo*[T](shader: Gluint, binding: Gluint): Ssbo[T] =
  glCreateBuffers(1, result.Gluint.addr)
  glBindBufferbase(GlShaderStorageBuffer, binding, result.Gluint)

proc copyTo*[T](val: T, ssbo: Ssbo[T]) =
  glBindBuffer(GlShaderStorageBuffer, ssbo.GLuint)
  glBufferData(GlShaderStorageBuffer, sizeof(T).GLsizeiptr, val.unsafeAddr, GlDynamicDraw)
  glBindBuffer(GlShaderStorageBuffer, 0.Gluint)


proc setUniform*(shader: Gluint, uniform: string, value: float32) =
  let loc = glGetUniformLocation(shader, uniform)
  if loc != -1:
    glUniform1f(loc, value.GlFloat)

proc setUniform*(shader: Gluint, uniform: string, value: int32) =
  let loc = glGetUniformLocation(shader, uniform)
  if loc != -1:
    glUniform1i(loc, value.Glint)
