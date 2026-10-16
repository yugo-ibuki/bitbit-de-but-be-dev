"""年季の入った木製チェスト(引き出し3段)を Blender (bpy) で生成してレンダリングする。

使い方:
    python build.py                 # 本番レンダリング (1600x1067)
    PREVIEW=1 python build.py       # 低解像度プレビュー
    OUT_DIR=/path python build.py   # 出力先の変更

マテリアルはすべてプロシージャル(外部テクスチャ不要)。
経年表現: 木目 / ニスのひび割れ / 擦り傷 / エッジの摩耗 / 隙間の汚れ(AO) /
         天板の埃 / 水の輪染み / 真鍮のくすみ / 剥がれた漆喰壁
"""
import math
import os
import random

import bpy  # noqa: I001  (bmesh より先に import する必要がある)
import bmesh
from mathutils import Vector

HERE = os.path.dirname(os.path.abspath(__file__))
OUT_DIR = os.environ.get("OUT_DIR", HERE)
PREVIEW = os.environ.get("PREVIEW") == "1"
random.seed(11)

# ---------------------------------------------------------------- scene reset
bpy.ops.wm.read_factory_settings(use_empty=True)
scene = bpy.context.scene
coll = scene.collection


# ---------------------------------------------------------------- node helpers
def lin(h):
    """sRGB hex -> linear RGBA"""
    h = h.lstrip("#")
    c = [int(h[i:i + 2], 16) / 255 for i in (0, 2, 4)]
    c = [x / 12.92 if x <= 0.04045 else ((x + 0.055) / 1.055) ** 2.4 for x in c]
    return (*c, 1.0)


class NB:
    def __init__(self, mat):
        mat.use_nodes = True
        self.nt = mat.node_tree
        self.nt.nodes.clear()

    def n(self, kind, **props):
        node = self.nt.nodes.new(kind)
        for k, v in props.items():
            setattr(node, k, v)
        return node

    def put(self, sock, v):
        if isinstance(v, bpy.types.NodeSocket):
            self.nt.links.new(v, sock)
        else:
            sock.default_value = v


def M(nb, op, a, b=None, c=None, clamp=False):
    n = nb.n("ShaderNodeMath", operation=op, use_clamp=clamp)
    for i, v in enumerate((a, b, c)):
        if v is not None:
            nb.put(n.inputs[i], v)
    return n.outputs[0]


def VM(nb, op, a, b=None):
    n = nb.n("ShaderNodeVectorMath", operation=op)
    nb.put(n.inputs[0], a)
    if b is not None:
        nb.put(n.inputs[1], b)
    return n


def MIX(nb, fac, a, b, blend="MIX"):
    n = nb.n("ShaderNodeMix", data_type="RGBA", blend_type=blend)
    nb.put(n.inputs[0], fac)
    nb.put(n.inputs[6], a)
    nb.put(n.inputs[7], b)
    return n.outputs[2]


def MIXF(nb, fac, a, b):
    """float lerp"""
    d = M(nb, "SUBTRACT", b, a)
    return M(nb, "MULTIPLY_ADD", d, fac, a)


def RAMP(nb, fac, stops):
    r = nb.n("ShaderNodeValToRGB")
    el = r.color_ramp.elements
    first, last = stops[0], stops[-1]
    el[0].position, el[0].color = first[0], _c(first[1])
    el[1].position, el[1].color = last[0], _c(last[1])
    for pos, col in stops[1:-1]:
        e = el.new(pos)
        e.color = _c(col)
    nb.put(r.inputs["Fac"], fac)
    return r.outputs["Color"]


def RAMPF(nb, fac, stops):
    """スカラー版(結果は R チャンネル)"""
    out = RAMP(nb, fac, [(p, (v, v, v, 1.0)) for p, v in stops])
    sep = nb.n("ShaderNodeSeparateColor")
    nb.nt.links.new(out, sep.inputs[0])
    return sep.outputs[0]


def _c(c):
    return c if len(c) == 4 else (*c, 1.0)


def NOISE(nb, vec, scale, detail=3.0, rough=0.5, dist=0.0):
    n = nb.n("ShaderNodeTexNoise", noise_dimensions="3D", normalize=True)
    nb.put(n.inputs["Vector"], vec)
    n.inputs["Scale"].default_value = scale
    n.inputs["Detail"].default_value = detail
    n.inputs["Roughness"].default_value = rough
    n.inputs["Distortion"].default_value = dist
    return n.outputs["Fac"]


def MAP(nb, vec, loc=(0, 0, 0), rot=(0, 0, 0), scale=(1, 1, 1)):
    m = nb.n("ShaderNodeMapping")
    nb.put(m.inputs["Vector"], vec)
    nb.put(m.inputs["Location"], loc)
    nb.put(m.inputs["Rotation"], rot)
    nb.put(m.inputs["Scale"], scale)
    return m.outputs["Vector"]


def AO(nb, dist, inside=False, local=False, samples=12):
    a = nb.n("ShaderNodeAmbientOcclusion", inside=inside, only_local=local, samples=samples)
    a.inputs["Distance"].default_value = dist
    return a.outputs["AO"]


def finish(nb, bsdf):
    out = nb.n("ShaderNodeOutputMaterial")
    nb.nt.links.new(bsdf.outputs["BSDF"], out.inputs["Surface"])


# ---------------------------------------------------------------- materials
def wood_material(name, axis, dark, mid, light, raw, rough=0.42, wear_amt=1.0,
                  dust_amt=0.55, crack_amt=1.0, scratch_amt=1.0, grime_amt=0.85,
                  rings=(), varnish=0.55, grain_scale=9.0, streak_lo=0.5):
    """axis: 木目が走る方向 (0=X, 1=Y, 2=Z / オブジェクトローカル)"""
    mat = bpy.data.materials.new(name)
    nb = NB(mat)
    tc = nb.n("ShaderNodeTexCoord")
    oi = nb.n("ShaderNodeObjectInfo")
    geo = nb.n("ShaderNodeNewGeometry")
    obj = tc.outputs["Object"]
    pos = geo.outputs["Position"]

    # オブジェクトごとに木目をずらす
    off = nb.n("ShaderNodeCombineXYZ")
    for i, k in enumerate(("X", "Y", "Z")):
        nb.put(off.inputs[k], M(nb, "MULTIPLY", oi.outputs["Random"], 7.3 + 3.1 * i))
    offv = off.outputs[0]

    cross = (axis + 1) % 3
    third = (axis + 2) % 3

    def stretched(along, across, other=None):
        s = [1.0, 1.0, 1.0]
        s[axis] = along
        s[cross] = across
        s[third] = across if other is None else other
        return MAP(nb, obj, loc=offv, scale=tuple(s))

    # --- 木目
    wave = nb.n("ShaderNodeTexWave", wave_type="BANDS", bands_direction="XYZ"[cross],
                wave_profile="SIN")
    nb.put(wave.inputs["Vector"], stretched(0.07, 1.0))
    wave.inputs["Scale"].default_value = grain_scale
    wave.inputs["Distortion"].default_value = 8.0
    wave.inputs["Detail"].default_value = 4.0
    wave.inputs["Detail Scale"].default_value = 1.6
    wave.inputs["Detail Roughness"].default_value = 0.6
    grain = wave.outputs["Fac"]

    streak = NOISE(nb, stretched(0.02, 1.0), 150, detail=4, rough=0.7)
    pores = NOISE(nb, stretched(0.012, 1.0), 260, detail=1, rough=0.5)

    col = RAMP(nb, grain, [(0.0, dark), (0.22, dark), (0.55, mid), (1.0, light)])
    col = MIX(nb, 1.0, col, RAMP(nb, streak, [(0.3, (streak_lo,) * 3), (0.7, (1.25,) * 3)]),
              "MULTIPLY")
    blotch = NOISE(nb, obj, 2.6, detail=5, rough=0.6, dist=0.4)
    col = MIX(nb, 0.45, col, RAMP(nb, blotch, [(0.3, (0.25,) * 3), (0.7, (0.75,) * 3)]),
              "OVERLAY")

    # --- 摩耗(エッジ): 凸エッジ = AO(inside) の暗部
    ao_tight = AO(nb, 0.007, inside=True, local=True, samples=16)
    ao_wide = AO(nb, 0.03, inside=True, local=True, samples=16)
    edge_t = RAMPF(nb, M(nb, "SUBTRACT", 1.0, ao_tight), [(0.04, 0.0), (0.30, 1.0)])
    edge_w = RAMPF(nb, M(nb, "SUBTRACT", 1.0, ao_wide), [(0.03, 0.0), (0.35, 1.0)])
    wnoise = RAMPF(nb, NOISE(nb, obj, 16, detail=5, rough=0.6), [(0.3, 0.15), (0.65, 1.0)])
    wear_raw = M(nb, "MULTIPLY", edge_t, wnoise, clamp=True)
    wear_raw = M(nb, "MULTIPLY", wear_raw, wear_amt, clamp=True)
    wear_soft = M(nb, "MULTIPLY", edge_w, wnoise, clamp=True)
    wear_soft = M(nb, "MULTIPLY", wear_soft, wear_amt, clamp=True)

    # ニスが剥げてニス下の明るい木肌が覗く
    col = MIX(nb, M(nb, "MULTIPLY", wear_soft, 0.22), col, MIX(nb, 0.6, light, raw))
    col = MIX(nb, wear_raw, col, MIX(nb, grain, raw, (0.95, 0.80, 0.58, 1.0), "MULTIPLY"))

    # --- 隙間・凹部の汚れ
    ao = AO(nb, 0.05, samples=12)
    grime = RAMPF(nb, ao, [(0.25, 1.0), (0.85, 0.0)])
    grime = M(nb, "MULTIPLY", grime, grime_amt, clamp=True)
    col = MIX(nb, grime, col, (0.012, 0.008, 0.005, 1.0))

    # --- 水の輪染み・汚れ(天面のみ)
    up = VM(nb, "DOT_PRODUCT", geo.outputs["Normal"], (0.0, 0.0, 1.0)).outputs["Value"]
    top_mask = RAMPF(nb, up, [(0.9, 0.0), (0.985, 1.0)])
    sep = nb.n("ShaderNodeSeparateXYZ")
    nb.nt.links.new(pos, sep.inputs[0])
    wob = M(nb, "MULTIPLY", M(nb, "SUBTRACT", NOISE(nb, pos, 22, detail=3), 0.5), 0.016)
    ring_total = None
    for (cx, cy, radius) in rings:
        dx = M(nb, "SUBTRACT", sep.outputs["X"], cx)
        dy = M(nb, "SUBTRACT", sep.outputs["Y"], cy)
        r = M(nb, "SQRT", M(nb, "ADD", M(nb, "POWER", dx, 2.0), M(nb, "POWER", dy, 2.0)))
        r = M(nb, "ADD", r, wob)
        rn = M(nb, "DIVIDE", r, radius * 1.25)
        ring = RAMPF(nb, rn, [(0.0, 0.16), (0.74, 0.20), (0.80, 0.95), (0.88, 0.75),
                              (0.95, 0.0), (1.0, 0.0)])
        brk = RAMPF(nb, NOISE(nb, pos, 40, detail=2), [(0.25, 0.1), (0.55, 1.0)])
        ring = M(nb, "MULTIPLY", ring, brk)
        ring_total = ring if ring_total is None else M(nb, "MAXIMUM", ring_total, ring)
    if ring_total is not None:
        ring_total = M(nb, "MULTIPLY", ring_total, top_mask, clamp=True)
        col = MIX(nb, M(nb, "MULTIPLY", ring_total, 0.55), col, (0.50, 0.42, 0.33, 1.0), "SCREEN")
    else:
        ring_total = 0.0

    # --- 埃
    dust_n = RAMPF(nb, NOISE(nb, pos, 7, detail=6, rough=0.6), [(0.28, 0.25), (0.7, 1.0)])
    dust = M(nb, "MULTIPLY", top_mask, dust_n)
    dust = M(nb, "MULTIPLY", dust, dust_amt, clamp=True)
    col = MIX(nb, M(nb, "MULTIPLY", dust, 0.75), col, (0.36, 0.32, 0.27, 1.0))

    # --- 擦り傷 / ニスのひび割れ
    def scratch_layer(rz, sc, thick, region_scale, seed):
        m = MAP(nb, obj, loc=(seed, seed * 0.7, seed * 1.3), rot=(0, 0, rz),
                scale=(0.35, sc, sc))
        nz = NOISE(nb, m, 4.0, detail=2, rough=0.5)
        line = RAMPF(nb, nz, [(0.5 - thick, 0.0), (0.5, 1.0), (0.5 + thick, 0.0)])
        region = RAMPF(nb, NOISE(nb, MAP(nb, obj, loc=(seed, 0, 0)), region_scale, detail=3),
                       [(0.42, 0.0), (0.62, 1.0)])
        return M(nb, "MULTIPLY", line, region)

    scr = M(nb, "MAXIMUM", scratch_layer(0.05, 28, 0.012, 3.0, 3.1),
            scratch_layer(-0.7, 34, 0.010, 4.0, 8.4))
    scr = M(nb, "MAXIMUM", scr, scratch_layer(1.9, 30, 0.009, 5.0, 12.2))
    scr = M(nb, "MULTIPLY", scr, scratch_amt, clamp=True)

    vor = nb.n("ShaderNodeTexVoronoi", feature="DISTANCE_TO_EDGE", voronoi_dimensions="3D")
    nb.put(vor.inputs["Vector"], stretched(0.05, 1.0))
    vor.inputs["Scale"].default_value = 38.0
    crack = RAMPF(nb, vor.outputs["Distance"], [(0.0, 1.0), (0.016, 0.0)])
    crack_region = RAMPF(nb, NOISE(nb, obj, 4, detail=3), [(0.52, 0.0), (0.72, 1.0)])
    crack = M(nb, "MULTIPLY", M(nb, "MULTIPLY", crack, crack_region), crack_amt, clamp=True)

    col = MIX(nb, M(nb, "MULTIPLY", scr, 0.55), col, MIX(nb, 0.4, raw, light))  # 傷は明るい
    col = MIX(nb, M(nb, "MULTIPLY", crack, 0.7), col, (0.015, 0.009, 0.005, 1.0))

    # --- 粗さ / ニス
    r_noise = RAMPF(nb, NOISE(nb, obj, 5, detail=4), [(0.3, max(rough - 0.14, 0.05)),
                                                      (0.7, rough + 0.2)])
    r = MIXF(nb, wear_soft, r_noise, 0.8)
    r = MIXF(nb, dust, r, 0.95)
    r = MIXF(nb, M(nb, "MULTIPLY", ring_total, 0.6), r, 0.7)
    r = MIXF(nb, grime, r, 0.9)

    coat_n = RAMPF(nb, NOISE(nb, obj, 3.5, detail=4, dist=0.5), [(0.3, 0.0), (0.62, 1.0)])
    coat = M(nb, "MULTIPLY", coat_n, varnish)
    coat = M(nb, "MULTIPLY", coat, M(nb, "SUBTRACT", 1.0, wear_soft))
    coat = M(nb, "MULTIPLY", coat, M(nb, "SUBTRACT", 1.0, dust))
    coat = M(nb, "MULTIPLY", coat, M(nb, "SUBTRACT", 1.0, M(nb, "MULTIPLY", crack, 0.7)))

    # --- 凹凸
    h = M(nb, "MULTIPLY_ADD", M(nb, "SUBTRACT", streak, 0.5), 0.35, 0.5)
    h = M(nb, "MULTIPLY_ADD", M(nb, "SUBTRACT", pores, 0.5), 0.12, h)
    h = M(nb, "SUBTRACT", h, M(nb, "MULTIPLY", scr, 0.45))
    h = M(nb, "SUBTRACT", h, M(nb, "MULTIPLY", crack, 0.55))
    h = M(nb, "SUBTRACT", h, M(nb, "MULTIPLY", wear_raw, 0.25))
    bump = nb.n("ShaderNodeBump")
    bump.inputs["Strength"].default_value = 0.85
    bump.inputs["Distance"].default_value = 0.0035
    nb.put(bump.inputs["Height"], h)

    bsdf = nb.n("ShaderNodeBsdfPrincipled")
    nb.put(bsdf.inputs["Base Color"], col)
    nb.put(bsdf.inputs["Roughness"], r)
    nb.put(bsdf.inputs["Coat Weight"], coat)
    bsdf.inputs["Coat Roughness"].default_value = 0.22
    nb.put(bsdf.inputs["Normal"], bump.outputs["Normal"])
    finish(nb, bsdf)
    return mat


def brass_material():
    mat = bpy.data.materials.new("brass_tarnished")
    nb = NB(mat)
    tc = nb.n("ShaderNodeTexCoord")
    obj = tc.outputs["Object"]
    crev = RAMPF(nb, AO(nb, 0.012, samples=16), [(0.2, 1.0), (0.85, 0.0)])
    patina_n = RAMPF(nb, NOISE(nb, obj, 60, detail=6, rough=0.65, dist=0.7),
                     [(0.25, 0.15), (0.55, 1.0)])
    patina = M(nb, "MAXIMUM", M(nb, "MULTIPLY", patina_n, 0.8), crev, clamp=True)
    col = MIX(nb, patina, lin("#a77c34"), lin("#1c1006"))
    verd = M(nb, "MULTIPLY", crev, RAMPF(nb, NOISE(nb, obj, 120, detail=3), [(0.35, 0), (0.6, 1)]))
    col = MIX(nb, M(nb, "MULTIPLY", verd, 0.7), col, lin("#2d5a4a"))
    rough = MIXF(nb, patina, RAMPF(nb, NOISE(nb, obj, 70, detail=4), [(0.3, 0.2), (0.7, 0.42)]),
                 0.7)
    sc = RAMPF(nb, NOISE(nb, MAP(nb, obj, scale=(1, 40, 40)), 6, detail=2),
               [(0.47, 0), (0.5, 1), (0.53, 0)])
    bump = nb.n("ShaderNodeBump")
    bump.inputs["Strength"].default_value = 0.4
    bump.inputs["Distance"].default_value = 0.0008
    nb.put(bump.inputs["Height"], M(nb, "SUBTRACT", 0.5, M(nb, "MULTIPLY", sc, 0.4)))
    bsdf = nb.n("ShaderNodeBsdfPrincipled")
    nb.put(bsdf.inputs["Base Color"], col)
    nb.put(bsdf.inputs["Metallic"], M(nb, "SUBTRACT", 1.0, M(nb, "MULTIPLY", patina, 0.65)))
    nb.put(bsdf.inputs["Roughness"], rough)
    nb.put(bsdf.inputs["Normal"], bump.outputs["Normal"])
    finish(nb, bsdf)
    return mat


def dark_material():
    mat = bpy.data.materials.new("keyhole_dark")
    nb = NB(mat)
    bsdf = nb.n("ShaderNodeBsdfPrincipled")
    bsdf.inputs["Base Color"].default_value = (0.004, 0.003, 0.002, 1)
    bsdf.inputs["Roughness"].default_value = 0.8
    finish(nb, bsdf)
    return mat


def plaster_material():
    """色褪せた緑のペンキが剥がれ、下の漆喰が覗く壁"""
    mat = bpy.data.materials.new("wall_peeling_paint")
    nb = NB(mat)
    geo = nb.n("ShaderNodeNewGeometry")
    pos = geo.outputs["Position"]
    peel_raw = NOISE(nb, MAP(nb, pos, scale=(1, 1, 1.3)), 1.9, detail=6, rough=0.58, dist=0.9)
    peel = RAMPF(nb, peel_raw, [(0.585, 0.0), (0.600, 1.0)])
    lip = M(nb, "MULTIPLY", RAMPF(nb, peel_raw, [(0.55, 0.0), (0.585, 1.0), (0.60, 0.0)]), 1.0)
    fine = NOISE(nb, pos, 55, detail=8, rough=0.7)
    paint = MIX(nb, RAMPF(nb, NOISE(nb, pos, 3, detail=4), [(0.3, 0.0), (0.7, 1.0)]),
                lin("#66725c"), lin("#848c74"))
    paint = MIX(nb, lip, paint, lin("#aeb59c"))
    under = MIX(nb, RAMPF(nb, NOISE(nb, pos, 6, detail=5), [(0.3, 0.0), (0.7, 1.0)]),
                lin("#a69a85"), lin("#7d725f"))
    col = MIX(nb, peel, paint, under)
    # 縦方向の雨だれ
    drip = RAMPF(nb, NOISE(nb, MAP(nb, pos, scale=(2.2, 2.2, 0.5)), 1.6, detail=4),
                 [(0.45, 0.0), (0.75, 1.0)])
    col = MIX(nb, M(nb, "MULTIPLY", drip, 0.30), col, lin("#2c241a"))
    col = MIX(nb, 0.35, col, RAMP(nb, fine, [(0.3, (0.7,) * 3), (0.7, (1.2,) * 3)]), "MULTIPLY")
    h = M(nb, "MULTIPLY_ADD", peel, -0.8, 1.0)
    h = M(nb, "MULTIPLY_ADD", M(nb, "SUBTRACT", fine, 0.5), 0.35, h)
    bump = nb.n("ShaderNodeBump")
    bump.inputs["Strength"].default_value = 0.9
    bump.inputs["Distance"].default_value = 0.012
    nb.put(bump.inputs["Height"], h)
    bsdf = nb.n("ShaderNodeBsdfPrincipled")
    nb.put(bsdf.inputs["Base Color"], col)
    bsdf.inputs["Roughness"].default_value = 0.88
    nb.put(bsdf.inputs["Normal"], bump.outputs["Normal"])
    finish(nb, bsdf)
    return mat


# material instances
RINGS = [(0.20, -0.09, 0.050), (0.255, -0.115, 0.044), (-0.27, 0.06, 0.07)]
MAT_BODY_Z = wood_material("oak_body_vertical", 2, lin("#1a0b05"), lin("#38200f"),
                           lin("#5c3a1f"), lin("#b88e5a"))
MAT_TOP = wood_material("oak_top", 0, lin("#1c0c06"), lin("#3a210f"), lin("#613d20"),
                        lin("#b88e5a"), rings=RINGS, dust_amt=0.9, wear_amt=1.3,
                        scratch_amt=1.6, crack_amt=1.2)
MAT_FRONT = wood_material("oak_drawer_front", 0, lin("#1a0b05"), lin("#39200f"),
                          lin("#5e3b1f"), lin("#b88e5a"), wear_amt=1.15)
MAT_PINE = wood_material("pine_unfinished", 0, lin("#7a5a35"), lin("#a07a48"),
                         lin("#c4a06a"), lin("#d8b985"), rough=0.7, wear_amt=0.4,
                         dust_amt=0.4, varnish=0.0, crack_amt=0.3, grain_scale=7)
MAT_PINE_Y = wood_material("pine_unfinished_y", 1, lin("#7a5a35"), lin("#a07a48"),
                           lin("#c4a06a"), lin("#d8b985"), rough=0.7, wear_amt=0.4,
                           dust_amt=0.4, varnish=0.0, crack_amt=0.3, grain_scale=7)
MAT_FLOOR = wood_material("floor_boards", 0, lin("#150c07"), lin("#30221a"), lin("#4c3a28"),
                          lin("#8a6c48"), rough=0.6, wear_amt=0.7, dust_amt=1.4,
                          varnish=0.25, grime_amt=0.95, grain_scale=7, streak_lo=0.7)
MAT_BASE = wood_material("baseboard", 0, lin("#20150d"), lin("#3a2a1c"), lin("#54402b"),
                         lin("#8a6c48"), rough=0.62, wear_amt=0.6, dust_amt=0.8,
                         varnish=0.1)
MAT_BRASS = brass_material()
MAT_DARK = dark_material()
MAT_WALL = plaster_material()


# ---------------------------------------------------------------- geometry
def _link(ob):
    coll.objects.link(ob)
    return ob


def smooth(ob):
    for p in ob.data.polygons:
        p.use_smooth = True


def box(name, size, loc, mat, bevel=0.003, rot=(0, 0, 0), seg=2):
    me = bpy.data.meshes.new(name)
    bm = bmesh.new()
    bmesh.ops.create_cube(bm, size=1.0)
    bmesh.ops.scale(bm, vec=size, verts=bm.verts)
    bm.to_mesh(me)
    bm.free()
    ob = _link(bpy.data.objects.new(name, me))
    ob.location = loc
    ob.rotation_euler = rot
    ob.data.materials.append(mat)
    smooth(ob)
    w = min(bevel, 0.4 * min(size))
    if w > 0:
        b = ob.modifiers.new("bevel", "BEVEL")
        b.width, b.segments, b.limit_method = w, seg, "ANGLE"
        b.angle_limit = math.radians(30)
        wn = ob.modifiers.new("wn", "WEIGHTED_NORMAL")
        wn.keep_sharp, wn.weight, wn.mode = True, 50, "FACE_AREA"
    return ob


def catmull(pts, sub=6):
    P = [pts[0]] + pts + [pts[-1]]
    out = []
    for i in range(1, len(P) - 2):
        p0, p1, p2, p3 = P[i - 1], P[i], P[i + 1], P[i + 2]
        for k in range(sub):
            t = k / sub
            out.append(tuple(
                0.5 * (2 * p1[j] + (-p0[j] + p2[j]) * t
                       + (2 * p0[j] - 5 * p1[j] + 4 * p2[j] - p3[j]) * t * t
                       + (-p0[j] + 3 * p1[j] - 3 * p2[j] + p3[j]) * t ** 3)
                for j in range(2)))
    out.append(pts[-1])
    return [(max(r, 0.0), z) for r, z in out]


def lathe(name, profile, loc, mat, rot=(0, 0, 0), steps=48, smooth_profile=True):
    prof = catmull(profile) if smooth_profile else profile
    bm = bmesh.new()
    vs = [bm.verts.new((r, 0, z)) for r, z in prof]
    es = [bm.edges.new((vs[i], vs[i + 1])) for i in range(len(vs) - 1)]
    bmesh.ops.spin(bm, geom=vs + es, cent=(0, 0, 0), axis=(0, 0, 1), angle=math.tau,
                   steps=steps, use_duplicate=False)
    bmesh.ops.remove_doubles(bm, verts=bm.verts, dist=1e-5)
    bmesh.ops.recalc_face_normals(bm, faces=bm.faces)
    me = bpy.data.meshes.new(name)
    bm.to_mesh(me)
    bm.free()
    ob = _link(bpy.data.objects.new(name, me))
    ob.location, ob.rotation_euler = loc, rot
    ob.data.materials.append(mat)
    smooth(ob)
    return ob


def jitter(a):
    return random.uniform(-a, a)


# ---------------------------------------------------------------- cabinet
W, D = 0.94, 0.44            # 本体 外寸
Y0 = -D / 2                  # 前面
T = 0.022                    # 板厚
Z_BOT, Z_TOP = 0.16, 0.80    # 本体の下端・上端
LEG_PROFILE = [(0, 0.0), (0.020, 0.0), (0.022, 0.006), (0.020, 0.016), (0.014, 0.025),
               (0.014, 0.033), (0.022, 0.050), (0.024, 0.062), (0.020, 0.078),
               (0.016, 0.086), (0.022, 0.091), (0.022, 0.097), (0, 0.097)]

# 脚(ターンド)
for sx in (-1, 1):
    for sy in (-1, 1):
        x, y = sx * (W / 2 - 0.022), sy * (D / 2 - 0.022)
        lathe(f"leg_{sx}_{sy}", LEG_PROFILE, (x, y, 0.0), MAT_BODY_Z)
        box(f"legblock_{sx}_{sy}", (0.044, 0.044, Z_BOT - 0.097 + 0.004), (x, y, 0.097 + (Z_BOT - 0.097) / 2 - 0.002),
            MAT_BODY_Z, bevel=0.0025)

# 側板・底板・背板・棚板
for sx in (-1, 1):
    box(f"side_{sx}", (T, D, Z_TOP - Z_BOT), (sx * (W / 2 - T / 2), 0, (Z_TOP + Z_BOT) / 2), MAT_BODY_Z)
    box(f"stile_{sx}", (0.032, T, Z_TOP - Z_BOT), (sx * (W / 2 - T - 0.016), Y0 + T / 2, (Z_TOP + Z_BOT) / 2),
        MAT_BODY_Z)
INNER_W = W - 2 * T
OPEN_W = INNER_W - 2 * 0.032      # 框の間の開口幅 (框と前面が同一平面で重ならないようにする)
box("bottom", (OPEN_W, D - 0.02, 0.03), (0, -0.01 + 0.0, Z_BOT + 0.015), MAT_BODY_Z)
box("back", (INNER_W, 0.008, Z_TOP - Z_BOT), (0, D / 2 - 0.004, (Z_TOP + Z_BOT) / 2), MAT_PINE_Y, bevel=0.001)
box("toprail", (OPEN_W, D - 0.02, 0.03), (0, -0.01, Z_TOP - 0.015), MAT_FRONT)
OPEN_B = Z_BOT + 0.03
DH = 0.18
for i in (0, 1):
    z = OPEN_B + DH + 0.01 + i * (DH + 0.02)
    box(f"shelf_{i}", (OPEN_W, D - 0.02, 0.02), (0, -0.01, z), MAT_FRONT)

# 天板(3枚はぎ)
TOP_W, TOP_D, TOP_T = 0.99, 0.50, 0.032
n_pl = 3
for i in range(n_pl):
    d = TOP_D / n_pl
    box(f"top_plank_{i}", (TOP_W, d - 0.0008, TOP_T),
        (jitter(0.0006), -TOP_D / 2 + d * (i + 0.5), Z_TOP + TOP_T / 2 + jitter(0.0007)),
        MAT_TOP, bevel=0.0035, rot=(jitter(0.0015), 0, jitter(0.0008)))

# 引き出し
FW = INNER_W - 2 * 0.032 - 0.005       # 前板の幅
FH = DH - 0.004
PULL = {1: 0.15}                        # 真ん中の引き出しを少し引き出す
for i in range(3):
    zc = OPEN_B + DH / 2 + i * (DH + 0.02) + jitter(0.0009)
    xc = jitter(0.0015)
    pull = PULL.get(i, jitter(0.0012))
    rz = jitter(0.0035) + (0.011 if i == 1 else 0.0)
    ry = -pull
    # 前板
    box(f"drawer_front_{i}", (FW, T, FH), (xc, Y0 + T / 2 + ry, zc), MAT_FRONT,
        bevel=0.0035, rot=(jitter(0.002), 0, rz))
    # 彫りの浅い羽目板 (段付き)
    box(f"drawer_lip_{i}", (FW - 0.075, 0.010, FH - 0.060), (xc, Y0 - 0.0035 + ry, zc), MAT_FRONT,
        bevel=0.0025, rot=(0, 0, rz))
    box(f"drawer_panel_{i}", (FW - 0.115, 0.014, FH - 0.100), (xc, Y0 - 0.006 + ry, zc), MAT_FRONT,
        bevel=0.004, rot=(0, 0, rz))
    # 引き出し本体(松材・無塗装)
    bw, bd, bh = FW - 0.045, 0.37, 0.115
    yb = Y0 + T + bd / 2 + ry
    zb = zc - FH / 2 + 0.02
    for sx in (-1, 1):
        box(f"drawer_side_{i}_{sx}", (0.012, bd, bh), (xc + sx * (bw / 2 - 0.006), yb, zb + bh / 2),
            MAT_PINE_Y, bevel=0.0015, rot=(0, 0, rz))
    box(f"drawer_back_{i}", (bw, 0.012, bh - 0.01), (xc, Y0 + T + bd - 0.006 + ry, zb + (bh - 0.01) / 2),
        MAT_PINE, bevel=0.0015, rot=(0, 0, rz))
    box(f"drawer_bottom_{i}", (bw - 0.01, bd, 0.008), (xc, yb, zb + 0.004), MAT_PINE_Y, bevel=0.001,
        rot=(0, 0, rz))

    # 真鍮の取っ手 (2つ) と鍵穴飾り
    face_y = Y0 - 0.013 + ry
    for sx in (-1, 1):
        kx = xc + sx * 0.21
        lathe(f"knob_{i}_{sx}",
              [(0, 0.0), (0.015, 0.0), (0.015, 0.0025), (0.0065, 0.004), (0.0058, 0.015),
               (0.010, 0.019), (0.0165, 0.026), (0.0165, 0.032), (0.011, 0.0385), (0, 0.040)],
              (kx, face_y, zc), MAT_BRASS, rot=(math.pi / 2, 0, 0), steps=40)
    lathe(f"escutcheon_{i}", [(0, 0.0), (0.0125, 0.0), (0.0125, 0.0022), (0.0095, 0.0032), (0, 0.0032)],
          (xc, face_y, zc + 0.02), MAT_BRASS, rot=(math.pi / 2, 0, 0), steps=32, smooth_profile=False)
    lathe(f"keyhole_{i}", [(0, 0.0), (0.0035, 0.0), (0.0035, 0.0036), (0, 0.0036)],
          (xc, face_y - 0.0004, zc + 0.0235), MAT_DARK, rot=(math.pi / 2, 0, 0), steps=16,
          smooth_profile=False)
    box(f"keyslot_{i}", (0.0026, 0.0007, 0.011), (xc, face_y - 0.0034, zc + 0.016), MAT_DARK, bevel=0)

# ---------------------------------------------------------------- room
BACK_Y = 0.272
box("wall", (9.0, 0.06, 3.6), (0, BACK_Y + 0.03, 1.5), MAT_WALL, bevel=0)
box("baseboard", (9.0, 0.02, 0.14), (0, BACK_Y - 0.01, 0.07), MAT_BASE, bevel=0.003)
box("baseboard_cap", (9.0, 0.012, 0.016), (0, BACK_Y - 0.016, 0.148), MAT_BASE, bevel=0.002)

# 床板
row_w, gap = 0.17, 0.0035
y = BACK_Y - 0.02
rows = 0
while y > -3.6:
    x = -4.5 - random.uniform(0, 1.2)
    while x < 4.5:
        L = random.uniform(0.9, 2.1)
        t = 0.02 + jitter(0.0006)
        box(f"floor_{rows}_{int(x * 10)}", (L - gap, row_w - gap, t),
            (x + L / 2, y - row_w / 2, -t / 2 + jitter(0.0004)), MAT_FLOOR, bevel=0.0025, seg=1,
            rot=(jitter(0.0008), jitter(0.0008), 0))
        x += L
    y -= row_w
    rows += 1
# 床の下(板間の隙間は黒)
box("subfloor", (12, 5, 0.02), (0, -1.6, -0.025), MAT_DARK, bevel=0)

# ---------------------------------------------------------------- light
sun_dir = Vector((0.70, -0.52, 0.50)).normalized()           # 太陽へ向かうベクトル
sun = bpy.data.objects.new("sun", bpy.data.lights.new("sun", "SUN"))
_link(sun)
sun.data.energy = float(os.environ.get("SUN", "7.5"))
sun.data.color = (1.0, 0.80, 0.56)
sun.data.angle = math.radians(0.9)
sun.rotation_euler = (-sun_dir).to_track_quat("-Z", "Y").to_euler()

# 窓の形の影を落とすオクルーダ (カメラ・間接光からは不可視)
occ_origin = sun_dir * 4.2 + Vector((0, 0, 0.0))
rq = sun_dir.to_track_quat("Z", "Y")
occluders = []


def occ_box(name, size, local):
    ob = box(name, size, (0, 0, 0), MAT_DARK, bevel=0)
    ob.location = occ_origin + rq @ Vector(local)
    ob.rotation_euler = rq.to_euler()
    for a in ("visible_camera", "visible_diffuse", "visible_glossy", "visible_transmission",
              "visible_volume_scatter"):
        setattr(ob, a, False)
    occluders.append(ob)


_p0 = Vector((0.0, -0.25, 0.45))                      # キャビネット前面の中心付近
_hit = _p0 + (4.2 - _p0.dot(sun_dir)) * sun_dir
_loc = rq.inverted() @ (_hit - occ_origin)
WX, WY = _loc.x + float(os.environ.get("WIN_DX", "0.55")), _loc.y + float(os.environ.get("WIN_DY", "0.1"))
WW, WH = 1.5, 2.0                                    # 窓の大きさ
BIG = 12.0
occ_box("occ_l", (BIG, BIG, 0.05), (WX - WW / 2 - BIG / 2, 0, 0))
occ_box("occ_r", (BIG, BIG, 0.05), (WX + WW / 2 + BIG / 2, 0, 0))
occ_box("occ_t", (WW, BIG, 0.05), (WX, WY + WH / 2 + BIG / 2, 0))
occ_box("occ_b", (WW, BIG, 0.05), (WX, WY - WH / 2 - BIG / 2, 0))
occ_box("occ_mv", (0.07, WH, 0.05), (WX, WY, 0))
occ_box("occ_mh1", (WW, 0.06, 0.05), (WX, WY + 0.33, 0))
occ_box("occ_mh2", (WW, 0.06, 0.05), (WX, WY - 0.45, 0))

# 柔らかい補助光(手前左)
fill = bpy.data.objects.new("fill", bpy.data.lights.new("fill", "AREA"))
_link(fill)
fill.data.energy = 90
fill.data.color = (0.70, 0.78, 1.0)
fill.data.shape, fill.data.size = "SQUARE", 3.0
fill.location = (-2.6, -2.4, 2.0)
fill.rotation_euler = (Vector((0.2, 0.0, 0.45)) - fill.location).to_track_quat("-Z", "Y").to_euler()

world = bpy.data.worlds.new("world")
scene.world = world
world.use_nodes = True
bg = world.node_tree.nodes["Background"]
bg.inputs["Color"].default_value = (0.42, 0.48, 0.58, 1)
bg.inputs["Strength"].default_value = 0.55

# ---------------------------------------------------------------- camera
cam_data = bpy.data.cameras.new("cam")
cam = _link(bpy.data.objects.new("cam", cam_data))
VIEW = os.environ.get("VIEW", "main")
if VIEW == "close":      # 質感確認用の接写
    cam.location = (-0.30, -1.25, 1.00)
    target = Vector((-0.05, -0.2, 0.62))
elif VIEW == "top":
    cam.location = (0.25, -1.1, 1.35)
    target = Vector((0.2, -0.1, 0.83))
else:
    cam.location = (-1.55, -2.85, 1.18)
    target = Vector((0.02, -0.05, 0.50))
cam.rotation_euler = (target - Vector(cam.location)).to_track_quat("-Z", "Y").to_euler()
cam_data.lens = 50
cam_data.dof.use_dof = True
cam_data.dof.focus_distance = (target - Vector(cam.location)).length
cam_data.dof.aperture_fstop = 6.3
scene.camera = cam

# ---------------------------------------------------------------- render
r = scene.render
r.engine = "CYCLES"
QUICK = os.environ.get("QUICK") == "1"
r.resolution_x, r.resolution_y = (480, 320) if QUICK else (800, 534) if PREVIEW else (1500, 1000)
r.resolution_percentage = 100
cy = scene.cycles
cy.device = "CPU"
if os.environ.get("DEVICE", "GPU") == "GPU":     # Metal が使えれば GPU、なければ CPU のまま
    prefs = bpy.context.preferences.addons["cycles"].preferences
    prefs.compute_device_type = "METAL"
    prefs.get_devices()
    gpus = [d for d in prefs.devices if d.type == "METAL"]
    for d in prefs.devices:
        d.use = d in gpus
    if gpus:
        cy.device = "GPU"
cy.preview_samples = 32                          # Rendered 表示を軽くする
cy.preview_denoiser = "OPENIMAGEDENOISE"
cy.use_preview_denoising = True
cy.samples = 14 if QUICK else 40 if PREVIEW else 120
cy.use_adaptive_sampling = True
cy.adaptive_threshold = 0.02
cy.use_denoising = True
cy.denoiser = "OPENIMAGEDENOISE"
cy.max_bounces = 6
cy.sample_clamp_indirect = 8.0
scene.view_settings.view_transform = "AgX"
for it in scene.view_settings.bl_rna.properties["look"].enum_items:
    if "Medium High Contrast" in it.name or "Medium High Contrast" in it.identifier:
        scene.view_settings.look = it.identifier
scene.view_settings.exposure = float(os.environ.get("EXPOSURE", "0.0"))

raw = os.path.join(OUT_DIR, "render_raw.png")
r.image_settings.file_format = "PNG"
r.filepath = raw
bpy.ops.wm.save_as_mainfile(filepath=os.path.join(OUT_DIR, "aged_cabinet.blend"))
bpy.ops.render.render(write_still=True)

# ---------------------------------------------------------------- post (vignette + grain)
import numpy as np
from PIL import Image

img = np.asarray(Image.open(raw).convert("RGB")).astype(np.float32) / 255.0
h, w, _ = img.shape
yy, xx = np.mgrid[0:h, 0:w]
d = np.sqrt(((xx - w / 2) / (w / 2)) ** 2 + ((yy - h / 2) / (h / 2)) ** 2)
img *= (1.0 - 0.28 * np.clip(d - 0.45, 0, 1) ** 1.6)[..., None]
img += np.random.default_rng(3).normal(0, 0.008, img.shape[:2])[..., None]
out = os.path.join(OUT_DIR, "render.png")
Image.fromarray((np.clip(img, 0, 1) * 255).astype(np.uint8)).save(out)
os.remove(raw)
print("saved", out)
