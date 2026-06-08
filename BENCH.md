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

# 示例：对比两个版本
bash bench_compare.sh /tmp/qjs-prev/qjs ./qjs

# 示例：对比四个版本（本次测试所用命令）
bash bench_compare.sh \
    /tmp/qjs-prev/qjs \
    ./qjs \
    "/path/to/mtqjs:mtqjs-master" \
    "/path/to/mtqjs-opt:mtqjs-opt"
```

输出为 Markdown 表格，非基准列显示分数及相对基准的百分比变化。

---

## 测试结果（2026-06-08）

测试环境：macOS ARM64 (Apple Silicon)

| 测试项 | 2025-09-13 | 2026-06-04 | mtqjs-master | mtqjs-opt |
|--------|-------:|-------:|-------:|-------:|
| Richards | 1234 | 1341 (+8.7%) | 1811 (+46.8%) | 1799 (+45.8%) |
| DeltaBlue | 1328 | 1526 (+14.9%) | 1878 (+41.4%) | 1795 (+35.2%) |
| Crypto | 1096 | 1181 (+7.8%) | 2134 (+94.7%) | 2120 (+93.4%) |
| RayTrace | 1649 | 3310 (+100.7%) | 3256 (+97.5%) | 2779 (+68.5%) |
| EarleyBoyer | 2751 | 4148 (+50.8%) | 4711 (+71.2%) | 3633 (+32.1%) |
| RegExp | 341 | 662 (+94.1%) | 444 (+30.2%) | 433 (+27.0%) |
| Splay | 3061 | 6919 (+126.0%) | 6022 (+96.7%) | 7269 (+137.5%) |
| NavierStokes | 1723 | 2161 (+25.4%) | 3589 (+108.3%) | 4060 (+135.6%) |
| **SCORE** | **1399** | **2063 (+47.5%)** | **2391 (+70.9%)** | **2335 (+66.9%)** |

> 分数为 V8 Benchmark Suite v7 规则下的几何平均值，**分数越高越好**。

---

## 结论

### quickjs 版本间对比（2025-09-13 → 2026-06-04）

总分提升 **+47.5%**，主要来自：

- **Splay +126%**、**RayTrace +101%**、**RegExp +94%**：新版本在对象内存分配（custom malloc for small blocks）和整数/浮点混合运算（faster add/sub/mul）上有大幅优化。
- Richards / DeltaBlue / NavierStokes：稳步提升（+8%～+25%）。

### mtqjs 与 quickjs 2026-06-04 对比

| 维度 | mtqjs-master | mtqjs-opt |
|------|:---:|:---:|
| 总分 vs qjs-2026 | **+15.9%** | **+13.2%** |
| 优势项 | Crypto、NavierStokes、EarleyBoyer | Crypto、NavierStokes、Splay |
| 弱势项 | RegExp (-33%)、RayTrace (-1.6%) | RegExp (-34%)、RayTrace (-16%) |

- **Crypto +94%、NavierStokes +108%**：mtqjs 在数值密集型场景有显著优势，推测受益于多线程或 JIT 相关优化。
- **RegExp** 是 mtqjs 的相对短板，两个变体均比 qjs-2026 低约 33%。
- **mtqjs-master vs mtqjs-opt**：两者总分相近（2391 vs 2335），master 在 EarleyBoyer 上更强，opt 在 Splay / NavierStokes 上更强。
