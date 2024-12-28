# Payroll Report Generator

## Setup

Make sure you have GNUCobol installed.

## Quick Start Zsh/Bash (Tested on WSL Ubuntu Zsh Windows 11 12/28/24)

Run the following commands:

```
git clone ...
cd PayrollReportGenerator
cobc -x payroll-report-generator.cob
./payroll-report-generator
cat emp-dat-fmt.txt
```

And you should see some formatted lines with author names and dollar amounts.
