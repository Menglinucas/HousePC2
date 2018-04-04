# HousePC
### Batch processing of interpolation.  
### Take house price as the research object.  
# Command
### 初次计算
```
HousePC::hp_CHN(startmon = '201104', endmon = '201106', resol = 200., configfile = 'city_info.txt', outpath = './result',  计算 sys = 'linux'/'wins', para = TRUE/FALSE)
```
### 后续单月计算
```
**HousePC::hp_CHNPost(mon = '201107', resol = 200., configfile = 'city_info.txt', outpath = './result', sys = 'linux'/'wins', para = TRUE/FALSE)**
```
