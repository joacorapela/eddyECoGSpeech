%How to call the 2D- SRNCP phase unwrapper from the C language
%You should have already compiled the phase unwrapper’s C code first 
%If you haven’t, to compile the C code: in the Matlab Command Window type
%         mex Miguel_2D_unwrapper.cpp
%The wrapped phase that you present as an input to the compiled C function 
%should have the single data type (float in C) 
WrappedPhase = single(image1_wrapped);
UnwrappedPhase = Miguel_2D_unwrapper(WrappedPhase);
figure, colormap(gray(256))
imagesc(UnwrappedPhase); 
xlabel('Pixels'), ylabel('Pixels')
title('Unwrapped phase image using the 2D- SRNCP algorithm')
figure 
surf(double(UnwrappedPhase),'FaceColor', 'interp', 'EdgeColor', 'none', 'FaceLighting', 'phong')
view(-30,30), camlight left, axis tight 
title('Unwrapped phase image using the 2D- SRNCP displayed as a surface')
xlabel('Pixels'), ylabel('Pixels'), zlabel('Phase in radians')

