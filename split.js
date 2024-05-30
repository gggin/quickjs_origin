const fs = require('fs');
const path = require('path');

function splitFile(inputFile, outputPrefix, chunkSizeKB) {
    const chunkSize = chunkSizeKB * 1024; // Convert KB to bytes
    let index = 1;

    // Open the input file as a stream
    const stream = fs.createReadStream(inputFile, { encoding: 'utf8', highWaterMark: chunkSize });

    stream.on('data', (chunk) => {
        const outputFile = `${outputPrefix}_part${index}.txt`;
        fs.writeFileSync(outputFile, chunk, 'utf8');
        index++;
    });

    stream.on('end', () => {
        console.log(`Splitting completed. ${index - 1} files created.`);
    });

    stream.on('error', (err) => {
        console.error('Error reading the input file:', err);
    });
}

// Example usage:
const inputFile = path.join(__dirname, 'quickjs.c');
const outputPrefix = 'output';
const chunkSizeKB = 800;

splitFile(inputFile, outputPrefix, chunkSizeKB);

