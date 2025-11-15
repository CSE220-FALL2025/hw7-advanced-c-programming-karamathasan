#include "hw7.h"

/// HELPERS
int isOp(char c)
{
    return (c == '\'' || c == '+' || c == '*');
}

int precedence(char op)
{
    if (op == '\'')
    {
        return 2;
    }
    else if (op == '*')
    {
        return 1;
    }
    else if (op == '+')
    {
        return 0;
    }
    else
        return -1;
}

bst_sf *insert_bst_sf(matrix_sf *mat, bst_sf *root)
{
    // given the root, compare the name of the root to the input matrix, and place it in the right or left subtree until it reaches a leaf

    // check if root is null
    if (root == NULL)
    {
        root = malloc(sizeof(bst_sf));//init new root
        root->mat = mat;
        root->right_child = NULL;
        root->left_child = NULL;
        
        return root;
    }

    // check right subtree for names greater than root, and left subtree for names less than root
    // no two matrices will have the same name, so there will be no checking for this case
    if ( mat->name > root->mat->name)
    {
        root->right_child = insert_bst_sf(mat, (root->right_child));
    }
    else
    {
        root->left_child = insert_bst_sf(mat, (root->left_child));
    }

    return root;
}

matrix_sf *find_bst_sf(char name, bst_sf *root)
{
    // no matrix if root is null
    if (root == NULL){
        return NULL;
    }

    // found
    if (name == root->mat->name)
    {
        return root->mat;
    }
    else if (name > root->mat->name)//name is greater, check right subtree
    {
        return find_bst_sf(name, (root->right_child));
    }
    else//name must be lower, check left subtree
    {
        return find_bst_sf(name, (root->left_child));
    }
}

void free_bst_sf(bst_sf *root)
{
    // do nothing if root is null
    if (root == NULL){
        return;
    }

    //free subtrees (if possible. if they are null, then nothing will happen)
    free_bst_sf(root->right_child);
    free_bst_sf(root->left_child);

    //free mat first to avoid dangling pointer
    free(root->mat);
    free(root);
}

matrix_sf *add_mats_sf(const matrix_sf *mat1, const matrix_sf *mat2)
{
    //init output matrix
    int rows = mat1->num_rows;
    int cols = mat2->num_cols;
    matrix_sf *out = malloc(sizeof(matrix_sf) + rows * cols * sizeof(int));
    out->num_rows = rows;
    out->num_cols = cols;
    out->name ='\0';

    //element wise summation
    for (int i = 0; i < rows; i++)
    {
        for (int j = 0; j < cols; j++)
        {
            *(out->values + i * cols + j) = *(mat1->values + i * cols + j) + *(mat2->values + i * cols + j);
        }
    }

    return out;
}

matrix_sf *mult_mats_sf(const matrix_sf *mat1, const matrix_sf *mat2)
{
    // consider mat1 to be MxN
    // consider mat2 to be NxP
    int m = mat1->num_rows;
    int n = mat1->num_cols;
    int p = mat2->num_cols;
    //output dim will be MxP
    matrix_sf *out = malloc(sizeof(matrix_sf) + m * p * sizeof(int));
    out->num_rows = m;
    out->num_cols = p;
    out->name = '\0';

    for (int i = 0; i < m; i++)
    {
        for (int j = 0; j < p; j++)
        {
            int sum = 0;
            for (int k = 0; k < n; k++)
            {
                sum += *(mat1->values + i * n + k) * *(mat2->values + k * p + j);//essentially a dot product
            }
            *(out->values + i * p + j) = sum;//sum of each dot product
        }
    }

    return out;
}

matrix_sf *transpose_mat_sf(const matrix_sf *mat)
{
    //output init
    int rows = mat->num_rows;
    int cols = mat->num_cols;
    matrix_sf *out = malloc(sizeof(matrix_sf) + rows * cols * sizeof(int));
    out->num_rows = cols;//transpose dims are swapped
    out->num_cols = rows;
    out->name = '\0';

    for (int i = 0; i < rows; i++)
    {
        for (int j = 0; j < cols; j++)
        {
            *(out->values + j * rows + i) = *(mat->values + i * cols + j);//swap indexing for transpose
        }
    }

    return out;
}

matrix_sf *create_matrix_sf(char name, const char *expr)
{
    int NR = 0;
    int NC = 0;
    
    sscanf(expr, "%d %d", &NR, &NC);//get dims
    expr = strchr(expr, '[') + 1;//skip to inside brackets
    //init output
    matrix_sf *out = malloc(sizeof(matrix_sf) + NR * NC * sizeof(int));
    out->name = name;
    out->num_rows = NR;
    out->num_cols = NC;

    for (int i = 0; i < NR; i++)
    {
        for (int j = 0; j < NC; j++)
        {
            int val = 0;
            int offset;
            sscanf(expr, "%d%n", &val, &offset);
            *(out->values + i * NC + j) = val;//add the value
            expr = expr + offset;//advance through the row
        }
        expr = strchr(expr, ';') + 1;// skip to next row
        
    }

    return out;
}

char *infix2postfix_sf(char *infix)
{
    int len = strlen(infix);
    char *p = infix;

    char *out = malloc(len * sizeof(char) + 1); // there is at most len + 1 characters in the final string, since parenthesis wont be preserved
    int steps = 0;

    char s[MAX_LINE_LEN];//mimic a stack with a list and index
    int top = -1;
    while (*p != '\0')
    {
        if (*p >= 'A' && *p <= 'Z')
        {
            *out = *p;//directly print characters when found
            out++;

            steps++;
        }
        else if (*p == '(')
        {
            //push
            top++;
            s[top] = *p;
            
        }
        else if (*p == ')')
        {
            while (s[top] != '(')
            {
                //pop
                *out = s[top];
                top--;
                out++;

                steps++;
            }
            top--;
        }
        else if (isOp(*p))
        {
            while (!(top <=-1) && (precedence(*p) <= precedence(s[top])))
            {
                //pop
                *out = s[top];
                top--;
                out++;

                steps++;
            }
            top++;
            s[top] = *p;
        }

        p++;
    }

    while (!(top <= -1))//retrieve all remaining elements from stack
    {
        *out = s[top];
        top--;
        out++;

        steps++;
    }
    *out = '\0';
    out -= steps;//move back by steps to print full string
    return out;
}

matrix_sf *evaluate_expr_sf(char name, char *expr, bst_sf *root)
{
    matrix_sf *s[MAX_LINE_LEN];//stack and index
    int top = -1;

    char *postfix = infix2postfix_sf(expr); // pointer to expression in postfix
    char *p = postfix;//copy pointer. makes memory freeing easier later
    while (*p != '\0')
    {
        if (*p >= 'A' && *p <= 'Z')//characters/matrices go into stack
        {
            top++;
            s[top] = find_bst_sf(*p, root);
        }
        else if (isOp(*p))
        {
            if (*p == '\'')//unary, pop one matrix
            {
                matrix_sf *mat = s[top];
                s[top] = transpose_mat_sf(mat);

                if (!find_bst_sf(mat->name, root)){//only free intermediate calculations if they are not on the bst
                    free(mat);
                }
            }
            else if (*p == '*')//binary pop two matrices
            {
                matrix_sf *right = s[top];
                top--;
                matrix_sf *left = s[top];
                // top--;
                s[top] = mult_mats_sf(left, right);

                //only free intermediate calculations if they are not on the bst
                if (!find_bst_sf(left->name, root)){
                    free(left);
                }
                if (!find_bst_sf(right->name, root)){
                    free(right);
                }
                
            }
            else if (*p == '+')
            {
                matrix_sf *right = s[top];
                top--;
                matrix_sf *left = s[top];
                // top--;
                s[top] = add_mats_sf(left, right);

                //only free intermediate calculations if they are not on the bst
                if (!find_bst_sf(left->name, root)){
                    free(left);
                }
                if (!find_bst_sf(right->name, root)){
                    free(right);
                }
            }
        }
        p++;
    }
    free(postfix);

    matrix_sf *out = s[top];
    out->name = name;
    return out;
}

matrix_sf *execute_script_sf(char *filename)
{
    FILE *file = fopen(filename, "r");
    char *buf = NULL;
    size_t max_line_size = MAX_LINE_LEN;

    bst_sf *bst = NULL;
    matrix_sf *result = NULL;
    while (getline(&buf, &max_line_size, file) != -1)//keep getting lines from the file to process
    {  
        char name;
        sscanf(buf, "%c", &name);//retrieve name 
        char * body = strchr(buf, '=') + 1;
        matrix_sf *mat = NULL;

        if (strchr(body, '['))//only matrix definitions have square brackets
        {
            mat = create_matrix_sf(name, body);
        }
        else//input files are well formatted, so no need to check 3rd case 
        {
            mat = evaluate_expr_sf(name, body, bst);
        }

        bst = insert_bst_sf(mat, bst);  
        result = mat;//store final result
    }
    result = copy_matrix(result->num_rows, result->num_cols, result->values);//self copy, new pointer
    free_bst_sf(bst);//copied matrix allows for bst to be freed with altering result variable
    free(buf);
    fclose(file);

    return result;
}

// This is a utility function used during testing. Feel free to adapt the code to implement some of
// the assignment. Feel equally free to ignore it.
matrix_sf *copy_matrix(unsigned int num_rows, unsigned int num_cols, int values[])
{
    matrix_sf *m = malloc(sizeof(matrix_sf) + num_rows * num_cols * sizeof(int));
    m->name = '?';
    m->num_rows = num_rows;
    m->num_cols = num_cols;
    memcpy(m->values, values, num_rows * num_cols * sizeof(int));
    return m;
}

// Don't touch this function. It's used by the testing framework.
// It's been left here in case it helps you debug and test your code.
void print_matrix_sf(matrix_sf *mat)
{
    assert(mat != NULL);
    assert(mat->num_rows <= 1000);
    assert(mat->num_cols <= 1000);
    printf("%d %d ", mat->num_rows, mat->num_cols);
    for (unsigned int i = 0; i < mat->num_rows * mat->num_cols; i++)
    {
        printf("%d", mat->values[i]);
        if (i < mat->num_rows * mat->num_cols - 1)
            printf(" ");
    }
    printf("\n");
}
