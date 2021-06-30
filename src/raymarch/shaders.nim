import opengl, vmath
import std/[tables]
import textures
const
  vertexShader = """
#version 430
layout (location = 0) in vec3 aPos;
  
void main()
{
    gl_Position = vec4(aPos, 1.0); // see how  directly give a vec3 to vec4's constructor
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

uniform sampler2D tilemap;
uniform vec4 tileinfo; // x = texelSize, y = tile count in axis, z = tile size

struct RayResult{
  int hit;
  vec3 normal;
  vec2 uv;
  vec3 pos;
};

uniform float MinStep = 0.01;

int getInd(ivec3 pos){
  int index = pos.x + pos.z * chunkSize + pos.y * chunkSize * chunkSize;
  return (ids[index / 2] >> (index % 2 * 0x10) & 0xffff); //int32 -> int16
}

vec2 getUvs( in vec3 ro, in vec3 rd) 
{
    vec3 m = 1.0/rd;
    vec3 n = m*ro;
    vec3 k = abs(m);
    vec3 t1 = -n - k;
    vec3 t2 = -n + k;
    vec2 oU;
    if( t1.x>t1.y && t1.x>t1.z ) { oU=ro.yz+rd.yz*t1.x;}
    else if( t1.y>t1.z   )       { oU=ro.zx+rd.zx*t1.y;}
    else                         { oU=ro.xy+rd.xy*t1.z;}
    return oU;
}

float shadowVal(vec3 pos)
{
  vec3 ray = normalize(-light.pos);

  ivec3 mapPos = ivec3(floor(pos));

  vec3 deltaDist = abs(vec3(length(ray)) / ray);

  ivec3 rayStep = ivec3(sign(ray));

  vec3 sideDist = (sign(ray) * (vec3(mapPos) - pos) + (sign(ray) * 0.5) + 0.5) * deltaDist; 
  bvec3 mask;

  mapPos += rayStep;
  for (int i = 0; i < camera.pos_dist.w; i++) {
    if (getInd(mapPos) > 0) return 1;
    mask = lessThanEqual(sideDist.xyz, min(sideDist.yzx, sideDist.zxy));
    
    sideDist += vec3(mask) * deltaDist;
    mapPos += ivec3(vec3(mask)) * rayStep;
  }

  return 1;
}

RayResult rayMarch(vec2 coord){
  RayResult result;
  vec3 ray = normalize(mat3(camera.matrix) * vec3(coord, 1));
  vec3 start = camera.pos_dist.xyz;

  ivec3 mapPos = ivec3(floor(start + 0.));

  vec3 deltaDist = abs(vec3(length(ray)) / ray);

  ivec3 rayStep = ivec3(sign(ray));

  vec3 sideDist = (sign(ray) * (vec3(mapPos) - start) + (sign(ray) * 0.5) + 0.5) * deltaDist; 
  bvec3 mask;

  for (int i = 0; i < camera.pos_dist.w; i++) {
    if (getInd(mapPos) > 0) break;
    mask = lessThanEqual(sideDist.xyz, min(sideDist.yzx, sideDist.zxy));
    
    sideDist += vec3(mask) * deltaDist;
    mapPos += ivec3(vec3(mask)) * rayStep;
  }
  result.hit = getInd(mapPos);
  if(result.hit > 0){
    result.pos = mapPos;
    if(mask.x){
      result.uv = abs(fract(start.zy + ray.zy * sideDist.x));
      result.normal = vec3(float(-rayStep.x), 0, 0);
    }else if(mask.y){
      result.uv = abs(fract(start.zx + ray.zx * sideDist.y));
      result.normal = vec3(0, float(-rayStep.y), 0);
    }else{
      result.uv = abs(fract(start.xy + ray.xy * sideDist.z));
      result.normal = vec3(0, 0, float(-rayStep.z));
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
    float light = dot(res.normal, normalize(lightPos)) * 0.5 + 0.5;
    int hit = res.hit - 1; //Air doesnt have a texture
    float spriteTexel = tileinfo.x * tileinfo.z;
    vec2 uv = vec2(hit % int(tileinfo.y), (hit / int(tileinfo.y))) * spriteTexel + res.uv * spriteTexel;
    vec4 col = texture(tilemap, uv);
    fragColor = vec4(col.rgb * light, 1);
    //fragColor = vec4(res.uv, 0, 1);

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

proc setUniform*(shader: Gluint, uniform: string, value: Vec4) =
  let loc = glGetUniformLocation(shader, uniform)
  if loc != -1:
    glUniform4f(loc, value.x, value.y, value.z, value.w)

proc setUniform*(shader: Gluint, uniform: string, tex: Texture) =
  let loc = glGetUniformLocation(shader, uniform)
  if loc != -1:
    let textureUnit = 0.Gluint;
    glBindTextureUnit(texture_unit, tex.GLuint);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST)
    glUniform1i(loc, textureUnit.Glint)
