function [ circle ] = createNiftiSphere( imageToMask, volumeToUse , centerOfSphere)
%CreateNiftiCricle is a function which wil be used to create a circle in
%the input images voxel dimensions with the volume to use as the volume of
%the sphere this will only create a 3d sphere

% First thing is we have to get the nifti's demensions


% Now lets draw a sphere with the correct volume
% Now lets draw a sphere with the correct volume
S=zeros(imageToMask.dim(2),imageToMask.dim(3),imageToMask.dim(4));
xLim=imageToMask.dim(2);
yLim=imageToMask.dim(3);
zLim=imageToMask.dim(4);
% Now find the radius
radius=((volumeToUse/pi)*(3/4))^1/3;
% Now lets declare anything we need to do for the center of the sphere
I = centerOfSphere(1);
J = centerOfSphere(2);
K = centerOfSphere(3);
% Find any differences between center and image center
% First find the center of the image
xCent = xLim / 2;
yCent = yLim / 2;
zCent = zLim / 2;

% Now find the difference between the  requested center and the image's
% center
xTrans = (xCent - I) * 2;
yTrans = (yCent - J) * 2;
zTrans = (zCent - K) * 2;


for i=1:xLim;for j=1:yLim;for k=1:zLim
  S(i,j,k)=(i-xTrans)/2)^2+(j-yTrans)/2)^2+(k-zTrans)/2)^2<=radius^2;
  %or <radius^2
end;end;end

circle = S;
end

