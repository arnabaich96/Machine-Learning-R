@echo off
setlocal

:: Change to the directory containing all the repositories
cd /d "E:/OneDrive - Florida State University/MyFSU_OneDrive/R-Codes/Machine Learning"

if %errorlevel% neq 0 (
    echo Failed to change directory to main repository.
    pause
    exit /b %errorlevel%
)

for /d %%i in (*) do (
    if exist "%%i\.git" (
        echo Pushing changes in %%i...
        pushd "%%i"
        git add .
        git commit -m s
        git push origin master  
        if %errorlevel% neq 0 (
            echo Failed to push changes for %%i.
            popd
            pause
            exit /b %errorlevel%
        )
        popd
    )
)

echo Done pushing all repositories.
pause
