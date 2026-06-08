# bench-v8 基准测试

## 前置条件

`tests/bench-v8/` 目录不在 git 仓库中，需从官方 extras 包解压：

```sh
curl -L -o /tmp/qjs-extras.tar.xz \
    https://bellard.org/quickjs/quickjs-extras-$(cat VERSION).tar.xz

tar -xJf /tmp/qjs-extras.tar.xz \
    --strip-components=1 \
    -C . \
    "quickjs-$(cat VERSION)/tests/bench-v8"
```

## 使用 bench_compare.sh

```sh
# 基本用法：一个或多个可执行文件，第一个为基准列
bash bench_compare.sh <qjs1> [qjs2 ...]

# 可选：用 path:label 指定列名（否则自动从二进制中提取版本号）
bash bench_compare.sh /old/qjs ./qjs /path/to/mtqjs:mtqjs-master

# 可选：用 -b <label> 指定基准列（默认为第一列）
bash bench_compare.sh -b mtqjs-master /old/qjs ./qjs /path/to/mtqjs:mtqjs-master

# 示例：对比两个版本
bash bench_compare.sh /tmp/qjs-prev/qjs ./qjs

# 示例：对比四个版本，以 mtqjs-master 为基准（本次测试所用命令）
bash bench_compare.sh -b mtqjs-master \
    /tmp/qjs-prev/qjs \
    ./qjs \
    "/path/to/mtqjs:mtqjs-master" \
    "/path/to/mtqjs-opt:mtqjs-opt"
```

输出为 Markdown 表格，非基准列显示分数及相对基准的百分比变化。

---

## 测试结果（2026-06-08）

测试环境：macOS ARM64 (Apple Silicon)  
基准列：**mtqjs-master**，其余列显示分数及相对基准的百分比变化。

| 测试项 | 2025-09-13 | 2026-06-04 | mtqjs-master | mtqjs-opt |
|--------|-------:|-------:|-------:|-------:|
| Richards | 1159 (-35.5%) | 1311 (-27.0%) | 1797 | 1758 (-2.2%) |
| DeltaBlue | 1199 (-29.8%) | 1461 (-14.4%) | 1707 | 1772 (+3.8%) |
| Crypto | 1029 (-51.1%) | 1122 (-46.7%) | 2105 | 2112 (+0.3%) |
| RayTrace | 1579 (-50.6%) | 3237 (+1.2%) | 3198 | 2724 (-14.8%) |
| EarleyBoyer | 2713 (-41.7%) | 3996 (-14.1%) | 4650 | 3651 (-21.5%) |
| RegExp | 327 (-23.8%) | 616 (+43.6%) | 429 | 426 (-0.7%) |
| Splay | 2928 (-52.0%) | 6576 (+7.7%) | 6106 | 7163 (+17.3%) |
| NavierStokes | 1645 (-54.3%) | 1997 (-44.5%) | 3597 | 4046 (+12.5%) |
| **SCORE** | **1327 (-43.3%)** | **1968 (-16.0%)** | **2342** | **2309 (-1.4%)** |

> 分数为 V8 Benchmark Suite v7 规则下的几何平均值，**分数越高越好**。

---

## 结论

### quickjs 版本间对比（2025-09-13 → 2026-06-04）

总分提升 **+48.3%**，主要来自：

- **Splay +125%**、**RayTrace +105%**、**RegExp +88%**：新版本在对象内存分配（custom malloc for small blocks）和整数/浮点混合运算（faster add/sub/mul）上有大幅优化。
- Richards / DeltaBlue / NavierStokes：稳步提升（+13%～+21%）。

### mtqjs 与 quickjs 2026-06-04 对比

| 维度             |           mtqjs-master            |              mtqjs-opt              |
| ---------------- | :-------------------------------: | :---------------------------------: |
| 总分 vs qjs-2026 |            **+19.0%**             |             **+17.3%**              |
| 优势项           | Crypto (+88%)、NavierStokes (+80%)、EarleyBoyer (+16%) | Crypto (+88%)、NavierStokes (+103%)、Splay (+9%) |
| 弱势项           |  RegExp (-30%)、RayTrace (-1.2%)  |    RegExp (-31%)、RayTrace (-16%)   |

- **Crypto +88%、NavierStokes +80%～+103%**：mtqjs 在数值密集型场景有显著优势，推测受益于多线程或 JIT 相关优化。
- **RegExp** 是 mtqjs 的相对短板，两个变体均比 qjs-2026 低约 30%。
- **mtqjs-master vs mtqjs-opt**：总分相近（2342 vs 2309），master 在 EarleyBoyer 上更强，opt 在 Splay / NavierStokes 上更强。
