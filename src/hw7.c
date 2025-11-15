#include "hw7.h"
// #include "string.h"

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
    if (root == NULL)
    {
        // *root = *mat;
        bst_sf *newRoot = malloc(sizeof(bst_sf));
        newRoot->mat = mat;
        *root = *newRoot;
        return root;
    }

    // no two matrices will have the same name, so there will be no checking for this case
    // if (strcmp(root->name, mat->name) > 0 ){
    if (root->mat->name > mat->name)
    {
        insert_bst_sf(mat, (root->left_child));
    }
    else
    {
        insert_bst_sf(mat, (root->right_child));
    }

    return root;
}

matrix_sf *find_bst_sf(char name, bst_sf *root)
{
    if (root->mat->name == name)
    {
        return root->mat;
    }
    else if (root->mat->name > name)
    {
        return find_bst_sf(name, (root->left_child));
    }
    else
    {
        return find_bst_sf(name, (root->right_child));
    }
}

void free_bst_sf(bst_sf *root)
{
    if ((root->right_child) != NULL)
    {
        free_bst_sf(root->right_child);
    }

    if ((root->left_child) != NULL)
    {
        free_bst_sf(root->left_child);
    }

    // TODO: check if root is a null pointer
    // TODO: make sure that freeing memory for a struct is this simple:
    //  dont i also have to free the memory of the members of the struct too?
    //  wouldnt i have dangling pointers otherwise?
    if ((root->right_child) == NULL && (root->left_child) == NULL)
    {
        // free(root->mat->values);
        // free(root->mat);
        free(root);
    }
}

matrix_sf *add_mats_sf(const matrix_sf *mat1, const matrix_sf *mat2)
{
    int rows = mat1->num_rows;
    int cols = mat2->num_cols;
    matrix_sf *out = malloc(sizeof(matrix_sf) + rows * cols * sizeof(int));
    for (int i = 0; i < rows; i++)
    {
        for (int j = 0; j < cols; j++)
        {
            // *(*((out->values) + i)+j) =  *(*((mat1->values)+i)+j) + *(*((mat2->values)+i)+j);
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

    matrix_sf *out = malloc(sizeof(matrix_sf) + m * p * sizeof(int));

    for (int i = 0; i < m; i++)
    {
        for (int j = 0; j < p; j++)
        {
            int sum = 0;
            for (int k = 0; k < n; k++)
            {
                // sum += *(*(mat1+i)+k) * *(*(mat2+k)+j);
                sum += *(mat1->values + i * n + k) * *(mat2->values + k * p + j);
            }
            // *(*(out->values+i)+j) = sum;
            *(out->values + i * p + j) = sum;
        }
    }

    return out;
}

matrix_sf *transpose_mat_sf(const matrix_sf *mat)
{
    int rows = mat->num_rows;
    int cols = mat->num_cols;
    matrix_sf *out = malloc(sizeof(matrix_sf) + rows * cols * sizeof(int));

    for (int i = 0; i < rows; i++)
    {
        for (int j = 0; j < cols; j++)
        {
            // *(*((out->values)+j)+i) = *(*((mat->values)+i)+j);
            *(out->values + j * rows + i) = *(mat->values + i * cols + j);
        }
    }

    return out;
}

matrix_sf *create_matrix_sf(char name, const char *expr)
{
    int NR = 0;
    int NC = 0;
    int offset = 0;
    sscanf(expr, "%d %d%n", &NR, &NC, &offset);
    expr += offset;

    matrix_sf *out = malloc(sizeof(matrix_sf) + NR * NC * sizeof(int));

    for (int i = 0; i < NR; i++)
    {
        for (int j = 0; j < NC; j++)
        {
            int val = 0;
            scanf(expr, "%d%n", &val, &offset);
            // *(*((out->values)+i)+j) = val;
            *(out->values + i * NC + j) = val;
            expr = expr + offset;
        }
    }

    return out;
}

char *infix2postfix_sf(char *infix)
{
    int len = strlen(infix);
    char *p = infix;
    char *out = malloc(len * sizeof(char) + 1); // there is at most len + 1 characters in the final string, since parenthesis wont be preserved
    int steps = 0;

    char s[MAX_LINE_LEN];
    int top = -1;
    while (*p != '\n')
    {
        int flag = 0;
        if (*p >= 'A' && *p < 'Z')
        {
            *out = *p;
            out++;

            steps++;
        }
        else if (*p == '(')
        {
            // push(s, *p);
            top++;
            s[top] = *p;
        }
        else if (*p == ')')
        {
            while (peek(s) == '(')
            {
                // *out = pop(s);
                *out = s[top];
                top--;
                out++;

                steps++;
            }
            // pop(s); // remove the starting parenthesis
            top--;
        }
        else if (isOp(*p))
        {
            while (!stackEmpty(s) && (precedence(*p) <= precedence(peek(s))))
            {
                // *out = pop(s);
                *out = s[top];
                top--;
                out++;

                steps++;
            }
            // push(s, *p);
            top++;
            s[top] = *p;
        }

        p++;
    }

    while (!stackEmpty(s))
    {
        // *out = pop(s);
        *out = p[top];
        top--;
        out++;

        steps++;
    }
    *out = '\n';
    out -= steps;
    return out;
}

matrix_sf *evaluate_expr_sf(char name, char *expr, bst_sf *root)
{
    matrix_sf *s[MAX_LINE_LEN];
    int top = -1;

    char *p = infix2postfix_sf(expr); // pointer to expression in postfix

    while (*p != '\n')
    {
        if (*p >= 'A' && *p < 'Z')
        {
            top++;
            s[top] = find_bst_sf(*p, root);
        }
        else if (isOp(*p))
        {
            if (*p == '\'')
            {
                matrix_sf *mat = s[top];
                s[top] = transpose_mat_sf(mat);
            }
            else if (*p == '*')
            {
                matrix_sf *right = s[top];
                top--;
                matrix_sf *left = s[top];
                // top--;
                s[top] = mult_mats_sf(left, right);
            }
            else if (*p == '+')
            {
                matrix_sf *right = s[top];
                top--;
                matrix_sf *left = s[top];
                // top--;
                s[top] = add_mats_sf(left, right);
            }
        }
    }

    matrix_sf *out = s[top];
    out->name = name;
    return out;
}

matrix_sf *execute_script_sf(char *filename)
{
    FILE *file = fopen(filename, "r");
    char *buf = NULL;
    size_t max_line_size = MAX_LINE_LEN;
    // getline(&str, &max_line_size, file);

    bst_sf *bst = malloc(sizeof(bst_sf));
    while (getline(&buf, &max_line_size, file) != EOF)
    {
        char name;
        int offset;
        sscanf(buf, "%c =%n", &name, &offset);
        buf += offset;
        matrix_sf *mat = NULL;

        int temp;
        if (sscanf(buf, "%d", &temp) != -1)
        {
            mat = create_matrix_sf(name, buf);
        }
        else
        {
            mat = evaluate_expr_sf(name, buf, bst);
        }
        insert_bst_sf(mat, bst);
    }

    free(buf);
    fclose(file);

    return NULL;
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
