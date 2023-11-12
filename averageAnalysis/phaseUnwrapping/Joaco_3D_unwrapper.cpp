//This program is written by Hussein Abdul-Rahman and Munther Gdeisat to program the three-dimensional phase unwrapper
//entitled "Fast three-dimensional phase-unwrapping algorithm based on sorting by 
//reliability following a noncontinuous path"
//by  Hussein Abdul-Rahman, Munther A. Gdeisat, David R. Burton, and Michael J. Lalor, 
//published in the Applied Optics, Vol. 46, No. 26, pp. 6623-6635, 2007.
//This program was written on 22nd August 2007
//The wrapped phase volume is floating point data type. Also, the unwrapped phase volume is foloating point
//read the data from the file frame by frame 
#include <malloc.h>
//#include "stdafx.h"
#include <stdlib.h>
#include<stdio.h>
#include <string.h>

static float PI = 3.141592654;
static float TWOPI = 6.283185307;
int No_of_edges = 0;

//voxel information
struct VOXEL
{
	//int x;					//x coordinate of the voxel
    //int y;					//y coordinate
	//int z;					//z coordinate
    int increment;			//No. of 2*pi to add to the voxel to unwrap it
    int number_of_voxels_in_group;	//No. of voxels in the voxel group
    float value;			//value of the voxel
	float reliability;
    int group;				//group No.
    int new_group;
    struct VOXEL *head;		//pointer to the first voxel in the group in the linked list
    struct VOXEL *last;		//pointer to the last voxel in the group
    struct VOXEL *next;		//pointer to the next voxel in the group
};

//the EDGE is the line that connects two voxels.
struct EDGE
{    
	float reliab;			//reliabilty of the edge and its equal to the sum of the reliability of the two voxels that it conntects
	VOXEL *pointer_1;		//pointer to the first voxel
    VOXEL *pointer_2;		//pointer to the second voxel
    int increment;			//No. of 2*pi to add to the voxel to unwrap it 
}; 

void read_data(char *inputfile,float *Data, int length)
{
	printf("Reading the Wrapped Values form Binary File.............>");
	FILE *ifptr;
	ifptr = fopen(inputfile,"rb");
	if(ifptr == NULL) printf("Error opening the file\n");
	fread(Data,sizeof(float),length,ifptr);
	fclose(ifptr);
	printf(" Done.\n");
}

void write_data(char *outputfile,float *Data,int length)
{
	printf("Writing the Unwrapped Values to Binary File.............>");
	FILE *ifptr;
	ifptr = fopen(outputfile,"wb");
	if(ifptr == NULL) printf("Error opening the file\n");
	fwrite(Data,sizeof(float),length,ifptr);
	fclose(ifptr);
	printf(" Done.\n");
}

//another version of Mixtogether but this function should only be use with the sort program
void  Mix(EDGE *Pointer1, int *index1, int *index2, int size)
{
	int counter1 = 0;
	int counter2 = 0;
	int *TemporalPointer = index1;

	int *Result = (int *) calloc(size * 2, sizeof(int));
	int *Follower = Result;

	while ((counter1 < size) && (counter2 < size))
	{
		if ((Pointer1[*(index1 + counter1)].reliab <= Pointer1[*(index2 + counter2)].reliab))
		{
			*Follower = *(index1 + counter1);
			Follower++;
			counter1++;
		} 
		else
        {
			*Follower = *(index2 + counter2);
			Follower++;
			counter2++;
        }
	}//while

	if (counter1 == size)
	{
		memcpy(Follower, (index2 + counter2), sizeof(int)*(size-counter2));
	} 
	else
	{
		memcpy(Follower, (index1 + counter1), sizeof(int)*(size-counter1));
	}

	Follower = Result;
	index1 = TemporalPointer;

	int i;
	for (i=0; i < 2 * size; i++)
	{
		*index1 = *Follower;
		index1++;
		Follower++;
	}

	free(Result);
}

//this is may be the fastest sort program; 
//see the explination in quickSort function below
void  sort(EDGE *Pointer, int *index, int size)
{
	if (size == 2)
	{
		if ((Pointer[*index].reliab) > (Pointer[*(index+1)].reliab))
		{
			int Temp;
			Temp = *index;
			*index = *(index+1);
			*(index+1) = Temp;
		}
	} 
	else if (size > 2)
    {
		sort(Pointer, index, size/2);
		sort(Pointer, (index + (size/2)), size/2);
		Mix(Pointer, index, (index + (size/2)), size/2);
    }
}

//this function tries to implement a nice idea explained below
//we need to sort edge array. Each edge element conisists of 16 bytes.
//In normal sort program we compare two elements in the array and exchange
//their place under some conditions to do the sorting. It is very probable
// that an edge element may change its place hundred of times which makes 
//the sorting a very time consuming operation. The idea in this function 
//is to give each edge element an index and move the index not the edge
//element. The edge need 4 bytes which makes the sorting operation faster.
// After finishingthe sorting of the indexes, we know the position of each index. 
//So we know how to sort edges
void  quick_sort(EDGE *Pointer, int size)
{
	int *index = (int *) calloc(size, sizeof(int));
	int i;

	for (i=0; i<size; ++i)
		index[i] = i;

	sort(Pointer, index, size);

	EDGE * a = (EDGE *) calloc(size, sizeof(EDGE));
	for (i=0; i<size; ++i)
		a[i] = Pointer[*(index + i)];

	memcpy(Pointer, a, size*sizeof(EDGE));

	free(index);
	free(a);
}

//---------------start quicker_sort algorithm --------------------------------
#define swap(x,y) {EDGE t; t=x; x=y; y=t;}
#define order(x,y) if (x.reliab > y.reliab) swap(x,y)
#define o2(x,y) order(x,y)
#define o3(x,y,z) o2(x,y); o2(x,z); o2(y,z)

typedef enum {yes, no} yes_no;

yes_no find_pivot(EDGE *left, EDGE *right, float *pivot_ptr)
{
	EDGE a, b, c, *p;

	a = *left;
	b = *(left + (right - left) /2 );
	c = *right;
	o3(a,b,c);

	if (a.reliab < b.reliab)
	{
		*pivot_ptr = b.reliab;
		return yes;
	}

	if (b.reliab < c.reliab)
	{
		*pivot_ptr = c.reliab;
		return yes;
	}

	for (p = left + 1; p <= right; ++p)
	{
		if (p->reliab != left->reliab)
		{
			*pivot_ptr = (p->reliab < left->reliab) ? left->reliab : p->reliab;
			return yes;
		}
		return no;
	}
}

EDGE *partition(EDGE *left, EDGE *right, float pivot)
{
	while (left <= right)
	{
		while (left->reliab < pivot)
			++left;
		while (right->reliab >= pivot)
			--right;
		if (left < right)
		{
			swap (*left, *right);
			++left;
			--right;
		}
	}
	return left;
}

void quicker_sort(EDGE *left, EDGE *right)
{
	EDGE *p;
	float pivot;

	if (find_pivot(left, right, &pivot) == yes)
	{
		p = partition(left, right, pivot);
		quicker_sort(left, p - 1);
		quicker_sort(p, right);
	}
}

//--------------end quicker_sort algorithm -----------------------------------

//--------------------start initialse voxels ----------------------------------
//initialse voxels. See the explination of the voxel class above.
//initially every voxel is a gorup by its self
//volume_width x direction, volume_height y direction, volume_depth z direction
void  initialiseVOXELs(float *WrappedVolume, VOXEL *voxel, int volume_width, int volume_height, int volume_depth)
{
	VOXEL *voxel_pointer = voxel;
	float *wrapped_volume_pointer = WrappedVolume;
	int n, i, j;

    for (n=0; n < volume_depth; n++)
	{
		for (i=0; i < volume_height; i++)
        {
			for (j=0; j < volume_width; j++)
			{
				//voxel_pointer->x = j;
  				//voxel_pointer->y = i;
				//voxel_pointer->z = n;
				voxel_pointer->increment = 0;
				voxel_pointer->number_of_voxels_in_group = 1;		
  				voxel_pointer->value = *wrapped_volume_pointer;
				voxel_pointer->reliability = 9999999+rand();
				voxel_pointer->head = voxel_pointer;
  				voxel_pointer->last = voxel_pointer;
				voxel_pointer->next = NULL;			
				voxel_pointer->new_group = 0;
				voxel_pointer->group = -1;
				voxel_pointer++;
				wrapped_volume_pointer++;
			}
         }
	}
}
//-------------------end initialise voxels -----------

//gamma function in the paper
float wrap(float voxel_value)
{
	float wrapped_voxel_value;
	if (voxel_value > PI)	wrapped_voxel_value = voxel_value - TWOPI;
	else if (voxel_value < -PI)	wrapped_voxel_value = voxel_value + TWOPI;
	else wrapped_voxel_value = voxel_value;
	return wrapped_voxel_value;
}

// voxelL_value is the left voxel,	voxelR_value is the right voxel
int find_wrap(float voxelL_value, float voxelR_value)
{
	float difference; 
	int wrap_value;
	difference = voxelL_value - voxelR_value;

	if (difference > PI)	wrap_value = -1;
	else if (difference < -PI)	wrap_value = 1;
	else wrap_value = 0;

	return wrap_value;
} 

void calculate_reliability(float *wrappedVolume, VOXEL *voxel, int volume_width, int volume_height, int volume_depth)
{
	int frame_size  = volume_width * volume_height;
	int volume_size = volume_width * volume_height * volume_depth;
	VOXEL *voxel_pointer;
	int index;
	float H, V, N, D1, D2, D3, D4, D5, D6, D7, D8, D9, D10;
	float *WVP;
	int n, i, j;
	
	WVP = wrappedVolume + frame_size + volume_width + 1;
	voxel_pointer = voxel + frame_size + volume_width + 1;
	for (n=1; n < volume_depth - 1; n++)
	{
		for (i=1; i < volume_height - 1; i++)
        {
			for (j=1; j < volume_width - 1; j++)
			{
				H  = wrap(*(WVP - 1) - *WVP) - wrap(*WVP - *(WVP + 1));
				V  = wrap(*(WVP - volume_width) - *WVP) - wrap(*WVP - *(WVP + volume_width));
				N  = wrap(*(WVP - frame_size) - *WVP) - wrap(*WVP - *(WVP + frame_size));
				D1 = wrap(*(WVP - volume_width - 1) - *WVP) - wrap(*WVP - *(WVP + volume_width + 1));
				D2 = wrap(*(WVP - volume_width + 1) - *WVP) - wrap(*WVP - *(WVP + volume_width - 1));
				D3 = wrap(*(WVP - frame_size - volume_width - 1) - *WVP) - wrap(*WVP - *(WVP + frame_size + volume_width + 1));
				D4 = wrap(*(WVP - frame_size - volume_width) - *WVP) - wrap(*WVP - *(WVP + frame_size + volume_width));
				D5 = wrap(*(WVP - frame_size - volume_width + 1) - *WVP) - wrap(*WVP - *(WVP + frame_size + volume_width - 1));
				D6 = wrap(*(WVP - frame_size - 1) - *WVP) - wrap(*WVP - *(WVP + frame_size + 1));
				D7 = wrap(*(WVP - frame_size + 1) - *WVP) - wrap(*WVP - *(WVP + frame_size - 1));
				D8 = wrap(*(WVP - frame_size + volume_width - 1) - *WVP) - wrap(*WVP - *(WVP + frame_size - volume_width + 1));
				D9 = wrap(*(WVP - frame_size + volume_width) - *WVP) - wrap(*WVP - *(WVP + frame_size - volume_width));
				D10 = wrap(*(WVP - frame_size + volume_width + 1) - *WVP) - wrap(*WVP - *(WVP + frame_size - volume_width - 1));
				voxel_pointer->reliability = H*H + V*V + N*N + D1*D1 + D2*D2  + D3*D3 + D4*D4  + D5*D5 + D6*D6  
					+ D7*D7 + D8*D8 + D9*D9 + D10*D10;
				voxel_pointer++;
				WVP++;
			}
			voxel_pointer += 2;
			WVP += 2;
		}
		voxel_pointer += 2 * volume_width;
		WVP += 2 * volume_width;
	}
}

//calculate the reliability of the horizental edges of the volume
//it is calculated by adding the reliability of voxel and the relibility of 
//its right neighbour
//edge is calculated between a voxel and its next neighbour
void  horizentalEDGEs(VOXEL *voxel, EDGE *edge, int volume_width, int volume_height, int volume_depth)
{
	int n, i, j;
	EDGE *edge_pointer = edge;
	VOXEL *voxel_pointer = voxel;
	
	for (n=0; n < volume_depth; n++)
	{
		for (i = 0; i < volume_height; i++)
		{
			for (j = 0; j < volume_width - 1; j++) 
			{
				edge_pointer->pointer_1 = voxel_pointer;
				edge_pointer->pointer_2 = (voxel_pointer+1);
				edge_pointer->reliab = voxel_pointer->reliability + (voxel_pointer + 1)->reliability;
				edge_pointer->increment = find_wrap(voxel_pointer->value, (voxel_pointer + 1)->value);
				voxel_pointer++;
				edge_pointer++;
				No_of_edges++;
			}
			voxel_pointer++;
		}
	}
}

void  verticalEDGEs(VOXEL *voxel, EDGE *edge, int volume_width, int volume_height, int volume_depth)
{
	int n, i, j;	
	VOXEL *voxel_pointer = voxel;
	EDGE *edge_pointer = edge + No_of_edges; 

	for (n=0; n < volume_depth; n++)
	{
		for (i=0; i<volume_height - 1; i++)
		{
			for (j=0; j < volume_width; j++) 
			{
				edge_pointer->pointer_1 = voxel_pointer;
				edge_pointer->pointer_2 = (voxel_pointer + volume_width);
				edge_pointer->reliab = voxel_pointer->reliability + (voxel_pointer + volume_width)->reliability;
				edge_pointer->increment = find_wrap(voxel_pointer->value, (voxel_pointer + volume_width)->value);
				voxel_pointer++;
				edge_pointer++;
				No_of_edges++;
			}
		}
		voxel_pointer += volume_width;
	} 
}

void  normalEDGEs(VOXEL *voxel, EDGE *edge, int volume_width, int volume_height, int volume_depth)
{
	int n, i, j;	
	int frame_size = volume_width * volume_height;
	VOXEL *voxel_pointer = voxel;
	EDGE *edge_pointer = edge + No_of_edges; 

	for (n=0; n < volume_depth - 1; n++)
	{
		for (i=0; i<volume_height; i++)
		{
			for (j=0; j < volume_width; j++) 
			{
				edge_pointer->pointer_1 = voxel_pointer;
				edge_pointer->pointer_2 = (voxel_pointer + frame_size);
				edge_pointer->reliab = voxel_pointer->reliability + (voxel_pointer + frame_size)->reliability;
				edge_pointer->increment = find_wrap(voxel_pointer->value, (voxel_pointer + frame_size)->value);
				voxel_pointer++;
				edge_pointer++;
				No_of_edges++;
			}
		}
	} 
}

//gather the voxels of the volume into groups 
void  gatherVOXELs(EDGE *edge, int volume_width, int volume_height, int volume_depth)
{
	int k; 
	VOXEL *VOXEL1;   
	VOXEL *VOXEL2;
	VOXEL *group1;
	VOXEL *group2;
	EDGE *pointer_edge = edge;
	int incremento;

	for (k = 0; k < No_of_edges; k++)
	{
		VOXEL1 = pointer_edge->pointer_1;
		VOXEL2 = pointer_edge->pointer_2;

		//VOXEL 1 and VOXEL 2 belong to different groups
		//initially each voxel is a group by it self and one voxel can construct a group
		//no else or else if to this if
		if (VOXEL2->head != VOXEL1->head)
		{
			//VOXEL 2 is alone in its group
			//merge this voxel with VOXEL 1 group and find the number of 2 pi to add 
			//to or subtract to unwrap it
			if ((VOXEL2->next == NULL) && (VOXEL2->head == VOXEL2))
			{
				VOXEL1->head->last->next = VOXEL2;
				VOXEL1->head->last = VOXEL2;
				(VOXEL1->head->number_of_voxels_in_group)++;
				VOXEL2->head=VOXEL1->head;
				VOXEL2->increment = VOXEL1->increment-pointer_edge->increment;
			}

			//VOXEL 1 is alone in its group
			//merge this voxel with VOXEL 2 group and find the number of 2 pi to add 
			//to or subtract to unwrap it
			else if ((VOXEL1->next == NULL) && (VOXEL1->head == VOXEL1))
			{
				VOXEL2->head->last->next = VOXEL1;
				VOXEL2->head->last = VOXEL1;
				(VOXEL2->head->number_of_voxels_in_group)++;
				VOXEL1->head = VOXEL2->head;
				VOXEL1->increment = VOXEL2->increment+pointer_edge->increment;
			} 

			//VOXEL 1 and VOXEL 2 both have groups
			else
            {
				group1 = VOXEL1->head;
                group2 = VOXEL2->head;
				//the no. of voxels in VOXEL 1 group is large than the no. of VOXELs
				//in VOXEL 2 group.   Merge VOXEL 2 group to VOXEL 1 group
				//and find the number of wraps between VOXEL 2 group and VOXEL 1 group
				//to unwrap VOXEL 2 group with respect to VOXEL 1 group.
				//the no. of wraps will be added to VOXEL 2 grop in the future
				if (group1->number_of_voxels_in_group > group2->number_of_voxels_in_group)
				{
					//merge VOXEL 2 with VOXEL 1 group
					group1->last->next = group2;
					group1->last = group2->last;
					group1->number_of_voxels_in_group = group1->number_of_voxels_in_group + group2->number_of_voxels_in_group;
					incremento = VOXEL1->increment-pointer_edge->increment - VOXEL2->increment;
					//merge the other voxels in VOXEL 2 group to VOXEL 1 group
					while (group2 != NULL)
					{
						group2->head = group1;
						group2->increment += incremento;
						group2 = group2->next;
					}
				} 

				//the no. of VOXELs in VOXEL 2 group is large than the no. of VOXELs
				//in VOXEL 1 group.   Merge VOXEL 1 group to VOXEL 2 group
				//and find the number of wraps between VOXEL 2 group and VOXEL 1 group
				//to unwrap VOXEL 1 group with respect to VOXEL 2 group.
				//the no. of wraps will be added to VOXEL 1 grop in the future
				else
                {
					//merge VOXEL 1 with VOXEL 2 group
					group2->last->next = group1;
					group2->last = group1->last;
					group2->number_of_voxels_in_group = group2->number_of_voxels_in_group + group1->number_of_voxels_in_group;
					incremento = VOXEL2->increment + pointer_edge->increment - VOXEL1->increment;
					//merge the other voxels in VOXEL 2 group to VOXEL 1 group
					while (group1 != NULL)
					{
						group1->head = group2;
						group1->increment += incremento;
						group1 = group1->next;
					} // while

                } // else
            } //else
        } ;//if
        pointer_edge++;
	}
} 

//unwrap the volume 
void  unwrapVolume(VOXEL *voxel, int volume_width, int volume_height, int volume_depth)
{
	int i;
	int volume_size = volume_width * volume_height * volume_depth;
	VOXEL *voxel_pointer = voxel;

	for (i = 0; i < volume_size; i++)
	{
		voxel_pointer->value += TWOPI * (float)(voxel_pointer->increment);
        voxel_pointer++;
    }
}

//the input to this unwrapper is an array that contains the wrapped phase map. 
//copy the volume on the buffer passed to this unwrapper to over write the unwrapped 
//phase map on the buffer of the wrapped phase map.
void  returnVolume(VOXEL *voxel, float *unwrappedVolume, int volume_width, int volume_height, int volume_depth)
{
	int i;
	int volume_size = volume_width * volume_height * volume_depth;
    float *unwrappedVolume_pointer = unwrappedVolume;
    VOXEL *voxel_pointer = voxel;

    for (i=0; i < volume_size; i++) 
	{
        *unwrappedVolume_pointer = voxel_pointer->value;
        voxel_pointer++;
		unwrappedVolume_pointer++;
	}
}


//the main function of the unwrapper
int main(int argc, char *argv[])
{ 
    if(argc!=4) {
        printf("Usage %s: <phase cube meta data filename> <input wrapped phase cube filename> <output unwrapped phase cube filename>\n", argv[0]);
        return 1;
    }
    int bufferSize = 80;
    FILE *fp;
    char buffer[bufferSize];
	float *WrappedVolume, *UnwrappedVolume;
    char *wrapped_volume_meta_data_name = argv[1];
    char *wrapped_volume_name = argv[2];
    char *unwrapped_volume_name = argv[3];
	int volume_width;
	int volume_height;
	int volume_depth;
	int volume_size;
	int two_volume_size = 2 * volume_size;

    fp = fopen(wrapped_volume_meta_data_name, "r");
    if (fp == NULL){
        printf("Could not open file %s", wrapped_volume_meta_data_name);
        return 1;
    }
    fgets(buffer, bufferSize, fp);
    volume_width = atoi(buffer);
    fgets(buffer, bufferSize, fp);
    volume_height = atoi(buffer);
    fgets(buffer, bufferSize, fp);
    volume_depth = atoi(buffer);
    fclose(fp);

	volume_size = volume_height * volume_width * volume_depth;
printf("width=%d, height=%d, depth=%d\n", volume_width, volume_height, volume_depth);
	int No_of_Edges_initially = 3 * volume_width * volume_height * volume_depth;
	WrappedVolume = (float *) calloc(volume_size, sizeof(float));
	read_data(wrapped_volume_name, WrappedVolume, volume_size);

printf("Showing 20 elements of WrappedVolume\n");
int i;
for(i=0; i<20; i++) {
    printf("%f\n", WrappedVolume[volume_size-1-i]);
}
	UnwrappedVolume = (float *) calloc(volume_size, sizeof(float));
	VOXEL *voxel = (VOXEL *) calloc(volume_size, sizeof(VOXEL));
	EDGE *edge = (EDGE *) calloc(No_of_Edges_initially, sizeof(EDGE));;

	initialiseVOXELs(WrappedVolume, voxel, volume_width, volume_height, volume_depth);

	calculate_reliability(WrappedVolume, voxel, volume_width, volume_height, volume_depth);

	horizentalEDGEs(voxel, edge, volume_width, volume_height, volume_depth);

	verticalEDGEs(voxel, edge, volume_width, volume_height, volume_depth);

	normalEDGEs(voxel, edge, volume_width, volume_height, volume_depth);

	//sort the EDGEs depending on their reiability. The PIXELs with higher relibility (small value) first
	//run only one of the two functions (quick_sort() or quicker_sort() )
	//if your code stuck because of the quicker_sort() function, then use the quick_sort() function
	quick_sort(edge, No_of_edges);
	//quicker_sort(edge, edge + No_of_edges - 1);
	
	//gather VOXELs into groups
	gatherVOXELs(edge, volume_width, volume_height, volume_depth);

	//unwrap the whole volume
	unwrapVolume(voxel, volume_width, volume_height, volume_depth);
	//copy the volume from VOXEL structure to the wrapped phase array passed to this function
	returnVolume(voxel, UnwrappedVolume, volume_width, volume_height, volume_depth);

	free(edge);
	free(voxel);

	write_data(unwrapped_volume_name,UnwrappedVolume, volume_size);
	free(UnwrappedVolume);
	free(WrappedVolume);
	getchar();
	return 1;
}
