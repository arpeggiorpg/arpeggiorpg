#!/usr/bin/env python3
"""Export arp3d's original hardcoded meshes into separate GLB files."""

import json
import struct
from pathlib import Path
from typing import Optional


class Mesh:
    def __init__(self, name: str):
        self.name = name
        self.positions: list[list[float]] = []
        self.colors: list[list[float]] = []
        self.indices: list[int] = []


def push_quad(mesh: Mesh, p0, p1, p2, p3, color):
    base = len(mesh.positions)
    mesh.positions.extend([p0, p1, p2, p3])
    mesh.colors.extend([color, color, color, color])
    mesh.indices.extend([base, base + 3, base + 1, base + 1, base + 3, base + 2])


def append_cube(mesh: Mesh, minp, maxp, top, side_light, side_dark, bottom):
    x0, y0, z0 = minp
    x1, y1, z1 = maxp
    push_quad(mesh, [x0, y1, z0], [x1, y1, z0], [x1, y1, z1], [x0, y1, z1], top)
    push_quad(mesh, [x0, y0, z0], [x0, y0, z1], [x1, y0, z1], [x1, y0, z0], bottom)
    push_quad(mesh, [x1, y0, z0], [x1, y0, z1], [x1, y1, z1], [x1, y1, z0], side_light)
    push_quad(mesh, [x0, y0, z0], [x0, y1, z0], [x0, y1, z1], [x0, y0, z1], side_dark)
    push_quad(mesh, [x0, y0, z1], [x0, y1, z1], [x1, y1, z1], [x1, y0, z1], side_light)
    push_quad(mesh, [x0, y0, z0], [x1, y0, z0], [x1, y1, z0], [x0, y1, z0], side_dark)


def push_triangle_double_sided(mesh: Mesh, p0, p1, p2, color):
    base = len(mesh.positions)
    mesh.positions.extend([p0, p1, p2])
    mesh.colors.extend([color, color, color])
    mesh.indices.extend([base, base + 1, base + 2, base, base + 2, base + 1])


def build_terrain_mesh() -> Mesh:
    mesh = Mesh("terrain_cube")
    append_cube(
        mesh,
        [0.0, 0.0, 0.0],
        [1.0, 1.0, 1.0],
        [1.0, 1.0, 1.0],
        [0.82, 0.82, 0.82],
        [0.68, 0.68, 0.68],
        [0.45, 0.45, 0.45],
    )
    return mesh


def build_creature_mesh() -> Mesh:
    mesh = Mesh("creature")

    footprint_x = 1.0
    footprint_z = 1.0
    height = 1.0
    center_x = 0.5
    center_z = 0.5
    base_y = 0.0

    leg_height = height * 0.42
    torso_height = height * 0.36
    head_height = height * 0.22
    arm_height = torso_height * 0.88

    leg_width = min(max(footprint_x * 0.22, 0.14), 0.28)
    leg_depth = min(max(footprint_z * 0.22, 0.14), 0.28)
    leg_gap = leg_width * 0.40

    torso_width = min(max(footprint_x * 0.52, 0.24), 0.44)
    torso_depth = min(max(footprint_z * 0.34, 0.18), 0.32)

    arm_width = min(max(leg_width * 0.90, 0.12), 0.22)
    arm_depth = min(max(leg_depth * 0.90, 0.12), 0.22)
    arm_offset = torso_width * 0.5 + arm_width * 0.5 + 0.02

    head_width = min(max(torso_width * 0.95, 0.22), 0.40)
    head_depth = min(max(torso_depth * 0.95, 0.18), 0.30)

    leg_top, leg_side_light, leg_side_dark, leg_bottom = (
        [0.22, 0.33, 0.80],
        [0.27, 0.38, 0.85],
        [0.18, 0.27, 0.70],
        [0.12, 0.18, 0.45],
    )
    torso_top, torso_side_light, torso_side_dark, torso_bottom = (
        [0.18, 0.58, 0.82],
        [0.22, 0.64, 0.88],
        [0.14, 0.46, 0.68],
        [0.10, 0.32, 0.48],
    )
    arm_top, arm_side_light, arm_side_dark, arm_bottom = (
        [0.85, 0.67, 0.54],
        [0.90, 0.72, 0.58],
        [0.76, 0.57, 0.45],
        [0.62, 0.47, 0.38],
    )
    head_top, head_side_light, head_side_dark, head_bottom = (
        [0.91, 0.74, 0.60],
        [0.96, 0.78, 0.64],
        [0.81, 0.63, 0.50],
        [0.66, 0.51, 0.40],
    )

    for leg_center_x in (
        center_x - (leg_width * 0.5 + leg_gap * 0.5),
        center_x + (leg_width * 0.5 + leg_gap * 0.5),
    ):
        append_cube(
            mesh,
            [leg_center_x - leg_width * 0.5, base_y, center_z - leg_depth * 0.5],
            [leg_center_x + leg_width * 0.5, base_y + leg_height, center_z + leg_depth * 0.5],
            leg_top,
            leg_side_light,
            leg_side_dark,
            leg_bottom,
        )

    torso_min_y = base_y + leg_height
    torso_max_y = torso_min_y + torso_height
    append_cube(
        mesh,
        [center_x - torso_width * 0.5, torso_min_y, center_z - torso_depth * 0.5],
        [center_x + torso_width * 0.5, torso_max_y, center_z + torso_depth * 0.5],
        torso_top,
        torso_side_light,
        torso_side_dark,
        torso_bottom,
    )

    arm_min_y = torso_min_y + torso_height * 0.08
    arm_max_y = arm_min_y + arm_height
    for arm_center_x in (center_x - arm_offset, center_x + arm_offset):
        append_cube(
            mesh,
            [arm_center_x - arm_width * 0.5, arm_min_y, center_z - arm_depth * 0.5],
            [arm_center_x + arm_width * 0.5, arm_max_y, center_z + arm_depth * 0.5],
            arm_top,
            arm_side_light,
            arm_side_dark,
            arm_bottom,
        )

    head_min_y = torso_max_y
    head_max_y = head_min_y + head_height
    append_cube(
        mesh,
        [center_x - head_width * 0.5, head_min_y, center_z - head_depth * 0.5],
        [center_x + head_width * 0.5, head_max_y, center_z + head_depth * 0.5],
        head_top,
        head_side_light,
        head_side_dark,
        head_bottom,
    )
    return mesh


def build_cursor_mesh() -> Mesh:
    mesh = Mesh("control_cursor")
    center = [0.0, 0.0, 0.0]
    radius = 1.0
    half_height = 1.0

    top = [center[0], center[1] + half_height, center[2]]
    bottom = [center[0], center[1] - half_height, center[2]]
    north = [center[0], center[1], center[2] - radius]
    south = [center[0], center[1], center[2] + radius]
    east = [center[0] + radius, center[1], center[2]]
    west = [center[0] - radius, center[1], center[2]]

    top_colors = ([0.72, 0.94, 1.0], [0.60, 0.88, 0.98], [0.50, 0.80, 0.94], [0.56, 0.84, 0.96])
    bottom_colors = (
        [0.24, 0.44, 0.72],
        [0.20, 0.37, 0.62],
        [0.16, 0.31, 0.53],
        [0.19, 0.34, 0.58],
    )

    push_triangle_double_sided(mesh, top, north, east, top_colors[0])
    push_triangle_double_sided(mesh, top, east, south, top_colors[1])
    push_triangle_double_sided(mesh, top, south, west, top_colors[2])
    push_triangle_double_sided(mesh, top, west, north, top_colors[3])

    push_triangle_double_sided(mesh, bottom, east, north, bottom_colors[0])
    push_triangle_double_sided(mesh, bottom, south, east, bottom_colors[1])
    push_triangle_double_sided(mesh, bottom, west, south, bottom_colors[2])
    push_triangle_double_sided(mesh, bottom, north, west, bottom_colors[3])

    return mesh


def f32_bytes(values):
    out = bytearray()
    for value in values:
        out.extend(struct.pack("<f", float(value)))
    return bytes(out)


def u32_bytes(values):
    out = bytearray()
    for value in values:
        out.extend(struct.pack("<I", int(value)))
    return bytes(out)


def write_glb(meshes: list[Mesh], out_path: Path):
    buffer = bytearray()
    buffer_views = []
    accessors = []
    gltf_meshes = []
    nodes = []

    def align4():
        while len(buffer) % 4 != 0:
            buffer.append(0)

    def add_view(raw: bytes, target: Optional[int] = None) -> int:
        align4()
        offset = len(buffer)
        buffer.extend(raw)
        view = {
            "buffer": 0,
            "byteOffset": offset,
            "byteLength": len(raw),
        }
        if target is not None:
            view["target"] = target
        buffer_views.append(view)
        return len(buffer_views) - 1

    def add_accessor(view_idx, component_type, count, accessor_type, minv=None, maxv=None):
        accessor = {
            "bufferView": view_idx,
            "componentType": component_type,
            "count": count,
            "type": accessor_type,
        }
        if minv is not None:
            accessor["min"] = minv
        if maxv is not None:
            accessor["max"] = maxv
        accessors.append(accessor)
        return len(accessors) - 1

    for mesh in meshes:
        pos_flat = [component for position in mesh.positions for component in position]
        col_flat = [component for color in mesh.colors for component in color]

        pos_view = add_view(f32_bytes(pos_flat), target=34962)
        col_view = add_view(f32_bytes(col_flat), target=34962)
        idx_view = add_view(u32_bytes(mesh.indices), target=34963)

        xs = [position[0] for position in mesh.positions]
        ys = [position[1] for position in mesh.positions]
        zs = [position[2] for position in mesh.positions]

        pos_accessor = add_accessor(
            pos_view,
            5126,
            len(mesh.positions),
            "VEC3",
            [min(xs), min(ys), min(zs)],
            [max(xs), max(ys), max(zs)],
        )
        color_accessor = add_accessor(col_view, 5126, len(mesh.colors), "VEC3")
        index_accessor = add_accessor(idx_view, 5125, len(mesh.indices), "SCALAR")

        gltf_meshes.append(
            {
                "name": mesh.name,
                "primitives": [
                    {
                        "attributes": {
                            "POSITION": pos_accessor,
                            "COLOR_0": color_accessor,
                        },
                        "indices": index_accessor,
                        "mode": 4,
                    }
                ],
            }
        )
        nodes.append({"name": mesh.name, "mesh": len(gltf_meshes) - 1})

    document = {
        "asset": {"version": "2.0", "generator": "arp3d hardcoded mesh extractor"},
        "buffers": [{"byteLength": len(buffer)}],
        "bufferViews": buffer_views,
        "accessors": accessors,
        "meshes": gltf_meshes,
        "nodes": nodes,
        "scenes": [{"nodes": list(range(len(nodes)))}],
        "scene": 0,
    }

    json_bytes = json.dumps(document, separators=(",", ":")).encode("utf-8")
    while len(json_bytes) % 4 != 0:
        json_bytes += b" "
    while len(buffer) % 4 != 0:
        buffer += b"\x00"

    total_length = 12 + 8 + len(json_bytes) + 8 + len(buffer)

    out = bytearray()
    out += struct.pack("<III", 0x46546C67, 2, total_length)
    out += struct.pack("<II", len(json_bytes), 0x4E4F534A)
    out += json_bytes
    out += struct.pack("<II", len(buffer), 0x004E4942)
    out += buffer

    out_path.parent.mkdir(parents=True, exist_ok=True)
    out_path.write_bytes(out)


def main():
    script_dir = Path(__file__).resolve().parent
    repo_root = script_dir.parent.parent
    out_dir = repo_root / "arpui" / "assets" / "models"
    model_meshes = [
        build_terrain_mesh(),
        build_creature_mesh(),
        build_cursor_mesh(),
    ]

    for mesh in model_meshes:
        out_path = out_dir / f"{mesh.name}.glb"
        write_glb([mesh], out_path)
        print(f"Wrote {out_path}")


if __name__ == "__main__":
    main()
