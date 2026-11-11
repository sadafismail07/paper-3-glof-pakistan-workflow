# Figure S3: Shisper drainage modes

import os, glob, shutil, tempfile, numpy as np, rasterio, geopandas as gpd, warnings; warnings.filterwarnings('ignore')
import pandas as pd, matplotlib; matplotlib.use('Agg'); import matplotlib.pyplot as plt
ROOT=os.getcwd(); P=ROOT+'/data/processed/'; M=ROOT+'/hecras_models/L27_Shisper_v2/'
plt.rcParams.update({'font.family':'DejaVu Sans','font.size':8})
def h4(folder):
    t=sorted(glob.glob(M+folder+'/*.tif')); dp=[x for x in t if 'Depth' in os.path.basename(x)][0]; vp=[x for x in t if 'Velocity' in os.path.basename(x)][0]
    with rasterio.open(dp) as r: d=r.read(1).astype(float); T=r.transform; crs=r.crs; nd=r.nodata
    with rasterio.open(vp) as r: v=r.read(1).astype(float)
    d[(d==nd)|(d<=0)]=np.nan
    return (np.isfinite(d)&((d>1.5)|(d*np.nan_to_num(v)>0.7))).astype('float32'), T, crs
A,T,crs=h4('L27_Mid'); B,_,_=h4('L27_ntfull'); C,_,_=h4('L27_tun2019')
fig,ax=plt.subplots(1,2,figsize=(17.4/2.54,8.2/2.54),gridspec_kw={'width_ratios':[1.15,1]})
# (a) hydrographs
cols={'breach':'#b2182b','fail':'#ef8a62','tun':'#2166ac'}
u=open(M+'L27_Shisper_GLOF.u01',errors='ignore').read().split('\n'); i0=[i for i,l in enumerate(u) if l.startswith('Flow Hydrograph=')][0]; n=int(u[i0].split('=')[1]); vals=[]
j=i0+1
while len(vals)<n: vals+= [float(u[j][k:k+8]) for k in range(0,len(u[j].rstrip('\r')),8) if u[j][k:k+8].strip()]; j+=1
itv=[l for l in u[:i0] if l.startswith('Interval=')][-1].split('=')[1].strip(); dt={'5SEC':5,'1MIN':60,'10SEC':10,'15SEC':15,'30SEC':30}[itv]
tb=np.arange(n)*dt/3600; qb=np.array(vals[:n])
ax[0].plot(tb,qb,color=cols['breach'],lw=1.4,label='Abrupt breach, full lake (%d m³ s⁻¹)'%round(qb.max()))
for f,lab,ls,cc in [('L27_ntfull','Ice-dam failure, full lake','-',cols['fail']),('L27_tunfull','Tunnel drainage, full lake','-',cols['tun']),('L27_tun2019','Tunnel drainage, 2019 volume','--',cols['tun']),('L27_tun2020','Tunnel drainage, 2020 volume',':',cols['tun'])]:
    d=pd.read_csv(P+'tunnel_hydrographs/'+f+'.csv'); tt=d.time_h.values-6; k=tt>=0
    ax[0].plot(np.maximum(tt[k],1e-3),d.Q_m3s.values[k],color=cc,ls=ls,lw=1.4,label='%s (%d m³ s⁻¹)'%(lab,round(d.Q_m3s.max())))
ax[0].set_xscale('log'); ax[0].set_yscale('log'); ax[0].set_xlim(0.01,72); ax[0].set_ylim(10,3000000)
ax[0].set_xlabel('Time since start of release (h)'); ax[0].set_ylabel('Inflow discharge (m³ s⁻¹)'); ax[0].grid(True,which='major',color='0.88',lw=0.5)
ax[0].legend(fontsize=5.8,loc='upper right',frameon=True,framealpha=0.95,title='Scenario (peak)',title_fontsize=6.5)
ax[0].set_title('(a) Inflow hydrographs',loc='left',fontweight='bold',fontsize=9)
# (b) width of the hazard zone along the ravine
from shapely.geometry import Point
from shapely.ops import substring
F=gpd.read_file(shutil.copy(P+'lake_flowpaths.gpkg',tempfile.mkdtemp())).to_crs(32642).set_index('lake_id'); g=F.geometry['L27']; s0=g.project(gpd.read_file('hecras_models/v2_inputs/L27_inflow_BC_v2.shp').to_crs(32642).geometry.union_all().centroid)
rav=substring(g,s0,s0+9000)
def width(m):
    rows,cols=np.where(m>0.5); xs,ys=rasterio.transform.xy(T,rows,cols)
    ok=[(x,y) for x,y in zip(xs,ys) if rav.distance(Point(x,y))<600]
    ch=np.array([rav.project(Point(x,y)) for x,y in ok]); ch=ch[(ch>0)&(ch<rav.length)]/1000
    bins=np.arange(0,9.01,0.5); h,_=np.histogram(ch,bins=bins); return bins[:-1]+0.25, h*900/500.0
for m,lab,cc,ls in [(A,'Abrupt breach, full lake',cols['breach'],'-'),(B,'Ice-dam failure, full lake',cols['fail'],'-'),(C,'Tunnel drainage, 2019 volume',cols['tun'],'--')]:
    x,wd=width(m); ax[1].plot(x,wd,color=cc,ls=ls,lw=1.4,marker='o',ms=2.5,label=lab)
ax[1].axvline(6.7,color='0.4',lw=0.8,ls=':'); ax[1].text(6.62,ax[1].get_ylim()[1]*0.97,'Highway bridge',rotation=90,va='top',ha='right',fontsize=6.5,color='0.3')
ax[1].axvline(8.9,color='0.4',lw=0.8,ls=':'); ax[1].text(8.82,ax[1].get_ylim()[1]*0.97,'Hunza confluence',rotation=90,va='top',ha='right',fontsize=6.5,color='0.3')
ax[1].set_xlim(0,9); ax[1].set_ylim(bottom=0); ax[1].set_xlabel('Distance below the inflow (km)'); ax[1].set_ylabel('Mean width of H4–H6 zone (m)'); ax[1].grid(True,color='0.88',lw=0.5)
ax[1].legend(fontsize=6.2,loc='upper left',framealpha=0.95)
ax[1].set_title('(b) Hazard-zone width in the ravine',loc='left',fontweight='bold',fontsize=9)
fig.tight_layout(w_pad=1.2)
out=ROOT+'/figures/'; os.makedirs(out,exist_ok=True)
fig.savefig(out+'Figure_S3.png',dpi=300); fig.savefig(out+'Figure_S3.tiff',dpi=600,pil_kwargs={'compression':'tiff_lzw'})
print('Saved figures/Figure_S3.png and .tiff')
