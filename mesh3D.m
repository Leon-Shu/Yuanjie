

function fig=mesh(a)

data = importdata('TEMP.txt');
X_Coord = data(:,1);
Y_Coord = data(:,2);
Z_Coord = -data(:,3);
%确定网格坐标（x和y方向的步长均取0.1）
[X,Y]=meshgrid(min(X_Coord):0.005:max(X_Coord),min(Y_Coord):0.005:max(Y_Coord)); 
%在网格点位置插值求Z，注意：不同的插值方法得到的曲线光滑度不同
Z=griddata(X_Coord,Y_Coord,Z_Coord,X,Y,'v4');

%绘制曲面

fig = figure(1);

surf(X,Y,Z);
shading interp;
colormap(jet);
% view(0, 90);
colorbar;

savefig('surface.fig')
end

