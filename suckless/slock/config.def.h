/* user and group to drop privileges to */
static const char *user  = "anon";
static const char *group = "anon";

static const char *colorname[NUMCOLS] = {
	[INIT] =   "black",     /* after initialization */
	[INPUT] =  "black",   /* during input */
	[FAILED] = "black",   /* wrong password */
};

/* treat a cleared input like a wrong password (color) */
static const int failonclear = 1;
