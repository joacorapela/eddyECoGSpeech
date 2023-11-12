%This program shows the problems encountered when unwrapping a noisy 2D phase 
%image by using computer simulation
clc; close all; clear
N = 512;
[x,y]=meshgrid(1:N);
noise_variance = 0.4;
image1 = 2*peaks(N) + 0.1*x + 0.01*y + noise_variance*randn(N,N);
figure, colormap(gray(256)), imagesc(image1)
title('Noisy continuous phase image displayed as visual intensity array')
xlabel('Pixels'), ylabel('Pixels')
figure 
surf(image1,'FaceColor', 'interp', 'EdgeColor', 'none', 'FaceLighting', 'phong')
view(-30,30), camlight left, axis tight 
title('Noisy continuous phase image displayed as a surface plot')
xlabel('Pixels'), ylabel('Pixels'), zlabel('Phase in radians')
figure, plot(image1(410,:))
title('Row 410 of the original noisy continuous phase image')
xlabel('Pixels'), ylabel('Phase in radians')
%wrap the 2D image
image1_wrapped = atan2(sin(image1), cos(image1));
fid = fopen('image1_wrapped.dat', 'w');
fwrite(fid, image1_wrapped, 'float');
fclose(fid);
figure, colormap(gray(256)), imagesc(image1_wrapped)
title('Noisy wrapped phase image displayed as visual intensity array')
xlabel('Pixels'), ylabel('Pixels')
figure 
surf(image1_wrapped,'FaceColor', 'interp', 'EdgeColor', 'none', 'FaceLighting', 'phong')
view(-30,70), camlight left, axis tight 
title('Noisy wrapped phase image plotted as a surface plot')
xlabel('Pixels'), ylabel('Pixels'), zlabel('Phase in radians')
figure, 
plot(image1_wrapped(410,:))
title('Row 410 of the wrapped noisy image')
xlabel('Pixels'), ylabel('Phase in radians')
%Unwrap the image using the Itoh algorithm: the first method
%Unwrap the image first by sequentially unwrapping the rows one at a time. 
image1_unwrapped =  image1_wrapped;
for i=1:N
    image1_unwrapped(i,:) = unwrap(image1_unwrapped(i,:));
end 
%Then unwrap all the columns one- by-  one 
for i=1:N
    image1_unwrapped(:,i) = unwrap(image1_unwrapped(:,i));
end 
figure, colormap(gray(256)), imagesc(image1_unwrapped)
title('Unwrapped noisy phase image using the Itoh algorithm: the first method')
xlabel('Pixels'), ylabel('Pixels')
figure 
surf(image1_unwrapped,'FaceColor', 'interp', 'EdgeColor', 'none', 'FaceLighting', 'phong')
view(-30,30), camlight left, axis tight 
title('Unwrapped noisy phase image using the Itoh unwrapper: the first method')
xlabel('Pixels'), ylabel('Pixels'), zlabel('Phase in radians')
%Unwrap the image using the Itoh algorithm: the second method
%Unwrap the image by first sequentially unwrapping all the columns. 
image2_unwrapped =  image1_wrapped;
for i=1:N
    image2_unwrapped(:,i) = unwrap(image2_unwrapped(:,i));
end 
%Then unwrap all the a rows one-by-one
for i=1:N
    image2_unwrapped(i,:) = unwrap(image2_unwrapped(i,:));
end
figure, colormap(gray(256)), imagesc(image2_unwrapped)
title('Unwrapped noisy image using the Itoh algorithm: the second method')
xlabel('Pixels'), ylabel('Pixels')
figure 
surf(image2_unwrapped,'FaceColor', 'interp', 'EdgeColor', 'none', 'FaceLighting', 'phong')
view(-30,30), camlight left, axis tight 
title('Unwrapped noisy phase image using the Itoh algorithm: the second method')
xlabel('Pixels'), ylabel('Pixels'), zlabel('Phase in radians')
